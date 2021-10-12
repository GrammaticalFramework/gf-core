#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <pthread.h>

#include "data.h"

PGF_INTERNAL __thread unsigned char* current_base __attribute__((tls_model("initial-exec"))) = NULL;
PGF_INTERNAL __thread PgfDB* current_db __attribute__((tls_model("initial-exec"))) = NULL;
PGF_INTERNAL __thread DB_scope *last_db_scope __attribute__((tls_model("initial-exec"))) = NULL;

#ifndef DEFAULT_TOP_PAD
#define DEFAULT_TOP_PAD        (0)
#endif

#define ptr(ms,o) ((mchunk*) (((char*) (ms)) + (o)))
#define ofs(ms,p) (((char*) (p)) - ((char*) (ms)))

struct mchunk {
  size_t mchunk_prev_size;  /* Size of previous chunk (if free).  */
  size_t mchunk_size;       /* Size in bytes, including overhead. */

  object fd;               /* double links -- used only if free. */
  object bk;

  /* Only used for large blocks: pointer to next larger size.     */
  object fd_nextsize;      /* double links -- used only if free. */
  object bk_nextsize;
};

#define POOL_ALIGNMENT (2 * sizeof(size_t) < __alignof__ (long double) \
                          ? __alignof__ (long double) : 2 * sizeof(size_t))

/*
   Bins
    An array of bin headers for free chunks. Each bin is doubly
    linked.  The bins are approximately proportionally (log) spaced.
    There are a lot of these bins (128). This may look excessive, but
    works very well in practice.  Most bins hold sizes that are
    unusual as allocation request sizes, but are more usual for fragments
    and consolidated sets of chunks, which is what these bins hold, so
    they can be found quickly.  All procedures maintain the invariant
    that no consolidated chunk physically borders another one, so each
    chunk in a list is known to be preceeded and followed by either
    inuse chunks or the ends of memory.
    Chunks in bins are kept in size order, with ties going to the
    approximately least recently used chunk. Ordering isn't needed
    for the small bins, which all contain the same-sized chunks, but
    facilitates best-fit allocation for larger chunks. These lists
    are just sequential. Keeping them in order almost never requires
    enough traversal to warrant using fancier ordered data
    structures.
    Chunks of the same size are linked with the most
    recently freed at the front, and allocations are taken from the
    back.  This results in LRU (FIFO) allocation order, which tends
    to give each chunk an equal opportunity to be consolidated with
    adjacent freed chunks, resulting in larger free chunks and less
    fragmentation.
    To simplify use in double-linked lists, each bin header acts
    as an mchunk. This avoids special-casing for headers.
    But to conserve space and improve locality, we allocate
    only the fd/bk pointers of bins, and then use repositioning tricks
    to treat these as the fields of a mchunk*.
 */

typedef struct mchunk mbin;

/* addressing -- note that bin_at(0) does not exist */
#define bin_at(m, i) \
  (mbin*) (((char *) &((m)->bins[((i) - 1) * 2]))                              \
           - offsetof (mchunk, fd))
/* analog of ++bin */
#define next_bin(b)  ((mbin*) ((char *) (b) + (sizeof(mchunk*) << 1)))
/* Reminders about list directionality within bins */
#define first(b)     ((b)->fd)
#define last(b)      ((b)->bk)

/*
   Indexing
    Bins for sizes < 512 bytes contain chunks of all the same size, spaced
    8 bytes apart. Larger bins are approximately logarithmically spaced:
    64 bins of size       8
    32 bins of size      64
    16 bins of size     512
     8 bins of size    4096
     4 bins of size   32768
     2 bins of size  262144
     1 bin  of size what's left
    There is actually a little bit of slop in the numbers in bin_index
    for the sake of speed. This makes no difference elsewhere.
    The bins top out around 1MB because we expect to service large
    requests via mmap.
    Bin 0 does not exist.  Bin 1 is the unordered list; if that would be
    a valid chunk size the small bins are bumped up one.
 */
#define NBINS             128
#define NSMALLBINS         64
#define SMALLBIN_WIDTH    POOL_ALIGNMENT
#define SMALLBIN_CORRECTION (POOL_ALIGNMENT > 2 * sizeof(size_t))
#define MIN_LARGE_SIZE    ((NSMALLBINS - SMALLBIN_CORRECTION) * SMALLBIN_WIDTH)

#define in_smallbin_range(sz)  \
  ((unsigned long) (sz) < (unsigned long) MIN_LARGE_SIZE)
#define smallbin_index(sz) \
  ((SMALLBIN_WIDTH == 16 ? (((unsigned) (sz)) >> 4) : (((unsigned) (sz)) >> 3))\
   + SMALLBIN_CORRECTION)
#define largebin_index_32(sz)                                                \
  (((((unsigned long) (sz)) >> 6) <= 38) ?  56 + (((unsigned long) (sz)) >> 6) :\
   ((((unsigned long) (sz)) >> 9) <= 20) ?  91 + (((unsigned long) (sz)) >> 9) :\
   ((((unsigned long) (sz)) >> 12) <= 10) ? 110 + (((unsigned long) (sz)) >> 12) :\
   ((((unsigned long) (sz)) >> 15) <= 4) ? 119 + (((unsigned long) (sz)) >> 15) :\
   ((((unsigned long) (sz)) >> 18) <= 2) ? 124 + (((unsigned long) (sz)) >> 18) :\
   126)
#define largebin_index_32_big(sz)                                            \
  (((((unsigned long) (sz)) >> 6) <= 45) ?  49 + (((unsigned long) (sz)) >> 6) :\
   ((((unsigned long) (sz)) >> 9) <= 20) ?  91 + (((unsigned long) (sz)) >> 9) :\
   ((((unsigned long) (sz)) >> 12) <= 10) ? 110 + (((unsigned long) (sz)) >> 12) :\
   ((((unsigned long) (sz)) >> 15) <= 4) ? 119 + (((unsigned long) (sz)) >> 15) :\
   ((((unsigned long) (sz)) >> 18) <= 2) ? 124 + (((unsigned long) (sz)) >> 18) :\
   126)
// XXX It remains to be seen whether it is good to keep the widths of
// XXX the buckets the same or whether it should be scaled by a factor
// XXX of two as well.
#define largebin_index_64(sz)                                                \
  (((((unsigned long) (sz)) >> 6) <= 48) ?  48 + (((unsigned long) (sz)) >> 6) :\
   ((((unsigned long) (sz)) >> 9) <= 20) ?  91 + (((unsigned long) (sz)) >> 9) :\
   ((((unsigned long) (sz)) >> 12) <= 10) ? 110 + (((unsigned long) (sz)) >> 12) :\
   ((((unsigned long) (sz)) >> 15) <= 4) ? 119 + (((unsigned long) (sz)) >> 15) :\
   ((((unsigned long) (sz)) >> 18) <= 2) ? 124 + (((unsigned long) (sz)) >> 18) :\
   126)
#define largebin_index(sz) \
  (sizeof(size_t) == 8 ? largebin_index_64 (sz)                            \
   : POOL_ALIGNMENT == 16 ? largebin_index_32_big (sz)                     \
   : largebin_index_32 (sz))


/*
   Unsorted chunks
    All remainders from chunk splits, as well as all returned chunks,
    are first placed in the "unsorted" bin. They are then placed
    in regular bins after malloc gives them ONE chance to be used before
    binning. So, basically, the unsorted_chunks list acts as a queue,
    with chunks being placed on it in free (and pool_consolidate),
    and taken off (to be either used or placed in bins) in malloc.
 */
/* The otherwise unindexable 1-bin is used to hold unsorted chunks. */
#define unsorted_chunks(M)          (bin_at (M, 1))

/* conversion from malloc headers to user pointers, and back */
#define chunk2mem(p)   ((void*)((char*)(p) + 2*sizeof(size_t)))
#define mem2chunk(mem) ((mchunk*)((char*)(mem) - 2*sizeof(size_t)))

#define MIN_CHUNK_SIZE (offsetof(mchunk, fd_nextsize))

/* The smallest size we can malloc is an aligned minimal chunk */
#define MINSIZE  \
  (unsigned long)(((MIN_CHUNK_SIZE+MALLOC_ALIGN_MASK) & ~MALLOC_ALIGN_MASK))

/* pad request bytes into a usable size -- internal version */
#define request2size(req)                                         \
  (((req) + sizeof(size_t) + MALLOC_ALIGN_MASK < MINSIZE)  ?             \
   MINSIZE :                                                      \
   ((req) + sizeof(size_t) + MALLOC_ALIGN_MASK) & ~MALLOC_ALIGN_MASK)

/*
   --------------- Physical chunk operations ---------------
 */
/* size field is or'ed with PREV_INUSE when previous adjacent chunk in use */
#define PREV_INUSE 0x1
/* extract inuse bit of previous chunk */
#define prev_inuse(p)       ((p)->mchunk_size & PREV_INUSE)

/* Get size, ignoring use bits */
#define chunksize(p) (p->mchunk_size & ~(PREV_INUSE))

/* Size of the chunk below P.  Only valid if !prev_inuse (P).  */
#define prev_size(p) ((p)->mchunk_prev_size)

/* Treat space at ptr + offset as a chunk */
#define chunk_at_offset(p, s)  ((mchunk*) (((char *) (p)) + (s)))

/* check/set/clear inuse bits in known places */
#define inuse_bit_at_offset(p, s)                                     \
  (((mchunk*) (((char *) (p)) + (s)))->mchunk_size & PREV_INUSE)

#define set_inuse_bit_at_offset(p, s)                                 \
  (((mchunk*) (((char *) (p)) + (s)))->mchunk_size |= PREV_INUSE)

#define clear_inuse_bit_at_offset(p, s)                               \
  (((mchunk*) (((char *) (p)) + (s)))->mchunk_size &= ~(PREV_INUSE))

/* Set size/use field */
#define set_head(p, s)       ((p)->mchunk_size = (s))
/* Set size at footer (only when chunk is not in use) */
#define set_foot(p, s)       (((mchunk*) ((char *) (p) + (s)))->mchunk_prev_size = (s))

/*
   Binmap
    To help compensate for the large number of bins, a one-level index
    structure is used for bin-by-bin searching.  `binmap' is a
    bitvector recording whether bins are definitely empty so they can
    be skipped over during during traversals.  The bits are NOT always
    cleared as soon as bins are empty, but instead only
    when they are noticed to be empty during traversal in malloc.
 */
/* Conservatively use 32 bits per map word, even if on 64bit system */
#define BINMAPSHIFT      5
#define BITSPERMAP       (1U << BINMAPSHIFT)
#define BINMAPSIZE       (NBINS / BITSPERMAP)

#define idx2block(i)     ((i) >> BINMAPSHIFT)
#define idx2bit(i)       ((1U << ((i) & ((1U << BINMAPSHIFT) - 1))))
#define mark_bin(ms, i)    ((ms)->binmap[idx2block(i)] |= idx2bit (i))
#define unmark_bin(ms, i)  ((ms)->binmap[idx2block(i)] &= ~(idx2bit (i)))
#define get_binmap(ms, i)  ((ms)->binmap[idx2block(i)] & idx2bit (i))

/*
   Fastbins
    An array of lists holding recently freed small chunks.  Fastbins
    are not doubly linked.  It is faster to single-link them, and
    since chunks are never removed from the middles of these lists,
    double linking is not necessary. Also, unlike regular bins, they
    are not even processed in FIFO order (they use faster LIFO) since
    ordering doesn't much matter in the transient contexts in which
    fastbins are normally used.
    Chunks in fastbins keep their inuse bit set, so they cannot
    be consolidated with other free chunks. malloc_consolidate
    releases all chunks in fastbins and consolidates them with
    other free chunks.
 */

#define DEFAULT_MXFAST    (64 * sizeof(size_t) / 4)

/* offset 2 to use otherwise unindexable first 2 bins */
#define fastbin_index(sz) \
  ((((unsigned int) (sz)) >> (sizeof(size_t) == 8 ? 4 : 3)) - 2)
/* The maximum fastbin request size we support */
#define MAX_FAST_SIZE     (80 * sizeof(size_t) / 4)
#define NFASTBINS    (fastbin_index (request2size (MAX_FAST_SIZE)) + 1)

/*
   FASTBIN_CONSOLIDATION_THRESHOLD is the size of a chunk in free()
   that triggers automatic consolidation of possibly-surrounding
   fastbin chunks. This is a heuristic, so the exact value should not
   matter too much. It is defined at half the default trim threshold as a
   compromise heuristic to only attempt consolidation if it is likely
   to lead to trimming. However, it is not dynamically tunable, since
   consolidation reduces fragmentation surrounding large chunks even
   if trimming is not used.
 */
#define FASTBIN_CONSOLIDATION_THRESHOLD  (65536UL)

static char slovo[5] = {'S','L','O','V','O'};

struct PGF_INTERNAL_DECL malloc_state
{
    /* Each .ngf file starts with 'SLOVO' as in:
     *    "V načaloto be slovoto" (In the beginning was the word)
     * In this way we detect an attempt to read a non .ngf file.
     */
    char sign[5];

    /* Set if the fastbin chunks contain recently inserted free blocks.  */
    bool have_fastchunks;
    /* Fastbins */
    object fastbins[NFASTBINS];
    /* Base of the topmost chunk -- not otherwise kept in a bin */
    object top;
    /* The remainder from the most recent split of a small request */
    object last_remainder;
    /* Normal bins packed as described above */
    object bins[NBINS * 2 - 2];
    /* Bitmap of bins */
    unsigned int binmap[BINMAPSIZE];

    /* The namespace of all persistant grammar revisions */
    Namespace<PgfPGF> revisions;

    /* A reference to the first transient revision in
     * a double-linked list.
     */
    ref<PgfPGF> transient_revisions;
};

PGF_INTERNAL
PgfDB::PgfDB(const char* filepath, int flags, int mode) {
    size_t file_size;
    bool is_new = false;

    fd = -1;
    ms = NULL;
    ref_count = 0;

    if (filepath == NULL) {
        this->filepath = NULL;
        file_size = getpagesize();
        is_new    = true;
    } else {
        fd = open(filepath, flags, mode);
        if (fd < 0)
            throw pgf_systemerror(errno, filepath);

        file_size = lseek(fd, 0, SEEK_END);
        if (file_size == ((off_t) -1)) {
            int code = errno;
            close(fd);
            throw pgf_systemerror(code, filepath);
        }

        is_new = false;
        if (file_size == 0) {
            file_size = getpagesize();
            if (ftruncate(fd, file_size) < 0) {
                int code = errno;
                close(fd);
                throw pgf_systemerror(code, filepath);
            }
            is_new = true;
        }

        this->filepath = strdup(filepath);
    }

    int mflags = (fd < 0) ? (MAP_PRIVATE | MAP_ANONYMOUS) : MAP_SHARED;
    ms = (malloc_state*)
        mmap(NULL, file_size, PROT_READ | PROT_WRITE, mflags, fd, 0);
    if (ms == MAP_FAILED) {
        ::free((void *) this->filepath);
        int code = errno;
        close(fd);
        throw pgf_systemerror(code, filepath);
    }

    int res = pthread_rwlock_init(&rwlock, NULL);
    if (res != 0) {
        ::free((void *) this->filepath);
        int code = errno;
        close(fd);
        throw pgf_systemerror(code);
    }

    if (is_new) {
        init_state(file_size);
    } else {
        if (strncmp(ms->sign, slovo, sizeof(ms->sign)) != 0) {
            ::free((void *) this->filepath);
            close(fd);
            throw pgf_error("Invalid file content");
        }

        // We must make sure that left-over transient revisions are
        // released. This may happen if a client process was killed
        // or if the garbadge collector has not managed to run
        // pgf_release_revision() before the process ended.

        while (ms->transient_revisions != 0) {
            pgf_free_revision(this, ms->transient_revisions.as_object());
        }
    }
}

PGF_INTERNAL
PgfDB::~PgfDB() {
    if (ms != NULL) {
        size_t size =
            ms->top + chunksize(ptr(ms,ms->top)) + sizeof(size_t);

        munmap(ms,size);
    }

    if (fd >= 0)
        close(fd);

    pthread_rwlock_destroy(&rwlock);

    ::free((void*) filepath);
}

PGF_INTERNAL
ref<PgfPGF> PgfDB::get_revision(PgfText *name)
{
    return namespace_lookup(current_db->ms->revisions, name);
}

PGF_INTERNAL
void PgfDB::set_revision(ref<PgfPGF> pgf)
{
    pgf->ref_count++;
    Namespace<PgfPGF> nmsp = namespace_insert(current_db->ms->revisions, pgf);
    namespace_release(current_db->ms->revisions);
    current_db->ms->revisions = nmsp;
}

PGF_INTERNAL
void PgfDB::init_state(size_t size)
{
    memcpy(ms->sign, slovo, sizeof(ms->sign));

    /* Init fastbins */
    ms->have_fastchunks = false;
    for (int i = 0; i < NFASTBINS; ++i) {
        ms->fastbins[i] = 0;
    }

    size_t sz = (sizeof(*ms) + sizeof(size_t));
    sz = (sz & ~MALLOC_ALIGN_MASK) + MALLOC_ALIGN_MASK + 1;

    mchunk* top_chunk = mem2chunk(((char*) ms) + sz);
    ms->top = ofs(ms,top_chunk);
    set_head(top_chunk, (size - (sz - sizeof(size_t))) | PREV_INUSE);

    ms->last_remainder = 0;

    /* Establish circular links for normal bins */
    for (int i = 1; i < NBINS; ++i) {
        mbin *bin = bin_at(ms, i);
        bin->fd = bin->bk = ofs(ms,bin);
    }

    memset(ms->binmap, 0, sizeof(ms->binmap));

    ms->revisions = 0;
    ms->transient_revisions = 0;
}

/* Take a chunk off a bin list.  */
static void
unlink_chunk (malloc_state* ms, mchunk* p)
{
    mchunk* fd = ptr(ms,p->fd);
    mchunk* bk = ptr(ms,p->bk);
    fd->bk = ofs(ms,bk);
    bk->fd = ofs(ms,fd);
    if (!in_smallbin_range(p->mchunk_size) && p->fd_nextsize != 0) {
        if (fd->fd_nextsize == 0) {
            if (p->fd_nextsize == ofs(ms,p))
                fd->fd_nextsize = fd->bk_nextsize = ofs(ms,fd);
            else {
                fd->fd_nextsize = p->fd_nextsize;
                fd->bk_nextsize = p->bk_nextsize;
                ptr(ms,p->fd_nextsize)->bk_nextsize = ofs(ms,fd);
                ptr(ms,p->bk_nextsize)->fd_nextsize = ofs(ms,fd);
            }
        } else {
            ptr(ms,p->fd_nextsize)->bk_nextsize = p->bk_nextsize;
            ptr(ms,p->bk_nextsize)->fd_nextsize = p->fd_nextsize;
        }
    }
}

/*
  ------------------------- malloc_consolidate -------------------------
  malloc_consolidate is a specialized version of free() that tears
  down chunks held in fastbins.  Free itself cannot be used for this
  purpose since, among other things, it might place chunks back onto
  fastbins.  So, instead, we need to use a minor variant of the same
  code.
*/
static void malloc_consolidate(malloc_state *ms)
{
  object* fb;                 /* current fastbin being consolidated */
  object* maxfb;              /* last fastbin (for loop control) */
  mchunk*  p;                 /* current chunk being consolidated */
  object   next_fb;           /* next chunk to consolidate */
  mchunk*  unsorted_bin;      /* bin header */
  mchunk*  first_unsorted;    /* chunk to link to */
  /* These have same use as in free() */
  mchunk*  nextchunk;
  size_t   size;
  size_t   nextsize;
  size_t   prevsize;
  int      nextinuse;

  ms->have_fastchunks = false;
  unsorted_bin = unsorted_chunks(ms);
  /*
    Remove each chunk from fast bin and consolidate it, placing it
    then in unsorted bin. Among other reasons for doing this,
    placing in unsorted bin avoids needing to calculate actual bins
    until malloc is sure that chunks aren't immediately going to be
    reused anyway.
  */
  maxfb = &ms->fastbins[NFASTBINS - 1];
  fb    = &ms->fastbins[0];
  do {
    if (*fb != 0) {
      p   = ptr(ms,*fb);
      *fb = 0;
      for (;;) {
        next_fb = p->fd;
        /* Slightly streamlined version of consolidation code in free() */
        size = chunksize(p);
        nextchunk = chunk_at_offset(p, size);
        nextsize = chunksize(nextchunk);
        if (!prev_inuse(p)) {
          prevsize = prev_size(p);
          size += prevsize;
          p = chunk_at_offset(p, -((long) prevsize));
          unlink_chunk (ms, p);
        }
        if (nextchunk != ptr(ms,ms->top)) {
          nextinuse = inuse_bit_at_offset(nextchunk, nextsize);
          if (!nextinuse) {
            size += nextsize;
            unlink_chunk (ms, nextchunk);
          } else
            clear_inuse_bit_at_offset(nextchunk, 0);
          first_unsorted = ptr(ms,unsorted_bin->fd);
          unsorted_bin->fd = ofs(ms,p);
          first_unsorted->bk = ofs(ms,p);
          if (!in_smallbin_range(size)) {
            p->fd_nextsize = 0;
            p->bk_nextsize = 0;
          }
          set_head(p, size | PREV_INUSE);
          p->bk = ofs(ms,unsorted_bin);
          p->fd = ofs(ms,first_unsorted);
          set_foot(p, size);
        } else {
          size += nextsize;
          set_head(p, size | PREV_INUSE);
          ms->top = ofs(ms,p);
        }

        if (next_fb == 0)
            break;
        p = ptr(ms,next_fb);
      }
    }
  } while (fb++ != maxfb);
}

PGF_INTERNAL
object PgfDB::malloc_internal(size_t bytes)
{
    unsigned int idx;                 /* associated bin index */
    mbin* bin;                        /* associated bin */
    mchunk* victim;                   /* inspected/selected chunk */

    mchunk* remainder;                /* remainder from a split */
    unsigned long remainder_size;     /* its size */

    /*
        Convert request size to internal form by adding SIZE_SZ bytes
        overhead plus possibly more to obtain necessary alignment and/or
        to obtain a size of at least MINSIZE, the smallest allocatable
        size. Also, checked_request2size traps (returning 0) request sizes
        that are so large that they wrap around zero when padded and
        aligned.
    */
    size_t nb = request2size(bytes);

    if (nb <= DEFAULT_MXFAST) {
        idx = fastbin_index(nb);

        if (ms->fastbins[idx] != 0) {
            victim = ptr(ms,ms->fastbins[idx]);
            ms->fastbins[idx] = victim->fd;
            return ofs(ms,chunk2mem(victim));
        }
    }

    /*
     If a small request, check regular bin.  Since these "smallbins"
     hold one size each, no searching within bins is necessary.
     (For a large request, we need to wait until unsorted chunks are
     processed to find best fit. But for small ones, fits are exact
     anyway, so we can check now, which is faster.)
   */
    if (in_smallbin_range (nb)) {
        idx = smallbin_index (nb);
        bin = bin_at (ms, idx);
        if ((victim = ptr(ms,last(bin))) != bin)
        {
            object bck = victim->bk;
            set_inuse_bit_at_offset (victim, nb);
            bin->bk = bck;
            ptr(ms,bck)->fd = ofs(ms,bin);
            return ofs(ms,chunk2mem(victim));
        }
    } else {
        /*
           If this is a large request, consolidate fastbins before continuing.
           While it might look excessive to kill all fastbins before
           even seeing if there is space available, this avoids
           fragmentation problems normally associated with fastbins.
           Also, in practice, programs tend to have runs of either small or
           large requests, but less often mixtures, so consolidation is not
           invoked all that often in most programs. And the programs that
           it is called frequently in otherwise tend to fragment.
        */

        idx = largebin_index(nb);
        if (ms->have_fastchunks)
            malloc_consolidate(ms);
    }

    /*
     Process recently freed or remaindered chunks, taking one only if
     it is exact fit, or, if this a small request, the chunk is remainder from
     the most recent non-exact fit.  Place other traversed chunks in
     bins.  Note that this step is the only place in any routine where
     chunks are placed in bins.
     The outer loop here is needed because we might not realize until
     near the end of malloc that we should have consolidated, so must
     do so and retry. This happens at most once, and only when we would
     otherwise need to expand memory to service a "small" request.
   */
    for (;;)
    {
        size_t size;
        mchunk *fwd, *bck;

        int iters = 0;
        while ((victim = ptr(ms,unsorted_chunks(ms)->bk)) != unsorted_chunks(ms)) {
            bck  = ptr(ms,victim->bk);
            size = chunksize(victim);
            mchunk *next = chunk_at_offset(victim, size);

            /*
             If a small request, try to use last remainder if it is the
             only chunk in unsorted bin.  This helps promote locality for
             runs of consecutive small requests. This is the only
             exception to best-fit, and applies only when there is
             no exact fit for a small chunk.
           */

            if (in_smallbin_range(nb) &&
                bck == unsorted_chunks(ms) &&
                victim == ptr(ms,ms->last_remainder) &&
                (unsigned long) (size) > (unsigned long) (nb + MINSIZE)) {

                /* split and reattach remainder */
                remainder_size = size - nb;
                remainder = chunk_at_offset(victim, nb);
                ms->last_remainder =
                  unsorted_chunks(ms)->bk =
                    unsorted_chunks(ms)->fd = ofs(ms,remainder);
                remainder->bk = remainder->fd = ofs(ms,unsorted_chunks(ms));
                if (!in_smallbin_range(remainder_size)) {
                    remainder->fd_nextsize = 0;
                    remainder->bk_nextsize = 0;
                }
                set_head(victim, nb | PREV_INUSE);
                set_head(remainder, remainder_size | PREV_INUSE);
                set_foot(remainder, remainder_size);
                return ofs(ms,chunk2mem(victim));
            }

            /* remove from unsorted list */
            unsorted_chunks(ms)->bk = ofs(ms,bck);
            bck->fd = ofs(ms,unsorted_chunks(ms));

            /* Take now instead of binning if exact fit */
            if (size == nb) {
                set_inuse_bit_at_offset(victim, size);
                return ofs(ms,chunk2mem(victim));
            }

            /* place chunk in bin */
            size_t victim_index;
            if (in_smallbin_range(size)) {
                victim_index = smallbin_index(size);
                bck = bin_at(ms, victim_index);
                fwd = ptr(ms,bck->fd);
            } else {
                victim_index = largebin_index(size);
                bck = bin_at(ms, victim_index);
                fwd = ptr(ms,bck->fd);

                /* maintain large bins in sorted order */
                if (fwd != bck) {
                    /* Or with inuse bit to speed comparisons */
                    size |= PREV_INUSE;
                    /* if smaller than smallest, bypass loop below */
                    if ((unsigned long) (size) < (unsigned long) ptr(ms,bck->bk)->mchunk_size) {
                        fwd = bck;
                        bck = ptr(ms,bck->bk);
                        victim->fd_nextsize = fwd->fd;
                        victim->bk_nextsize = ptr(ms,fwd->fd)->bk_nextsize;
                        ptr(ms,fwd->fd)->bk_nextsize = ptr(ms,victim->bk_nextsize)->fd_nextsize = ofs(ms,victim);
                    } else {
                        while ((unsigned long) size < fwd->mchunk_size) {
                            fwd = ptr(ms,fwd->fd_nextsize);
                        }
                        if ((unsigned long) size == (unsigned long) fwd->mchunk_size)
                            /* Always insert in the second position. */
                            fwd = ptr(ms,fwd->fd);
                        else {
                            victim->fd_nextsize = ofs(ms,fwd);
                            victim->bk_nextsize = fwd->bk_nextsize;
                            fwd->bk_nextsize = ofs(ms,victim);
                            ptr(ms,victim->bk_nextsize)->fd_nextsize = ofs(ms,victim);
                        }
                        bck = ptr(ms,fwd->bk);
                    }
                } else {
                    victim->fd_nextsize = victim->bk_nextsize = ofs(ms,victim);
                }
            }

            mark_bin(ms, victim_index);
            victim->bk = ofs(ms,bck);
            victim->fd = ofs(ms,fwd);
            fwd->bk = ofs(ms,victim);
            bck->fd = ofs(ms,victim);

#define MAX_ITERS 10000
            if (++iters >= MAX_ITERS)
                break;
        }

        /*
         If a large request, scan through the chunks of current bin in
         sorted order to find smallest that fits.  Use the skip list for this.
        */
        if (!in_smallbin_range(nb)) {
            bin = bin_at(ms, idx);

            /* skip scan if empty or largest chunk is too small */
            if ((victim = ptr(ms,first(bin))) != bin &&
                (unsigned long) victim->mchunk_size >= (unsigned long) (nb)) {
                size_t size;

                victim = ptr(ms,victim->bk_nextsize);
                while (((unsigned long) (size = chunksize(victim)) <
                       (unsigned long) (nb)))
                    victim = ptr(ms,victim->bk_nextsize);

                /* Avoid removing the first entry for a size so that the skip
                   list does not have to be rerouted.  */
                if (victim != ptr(ms,last(bin)) &&
                    victim->mchunk_size == ptr(ms,victim->fd)->mchunk_size)
                  victim = ptr(ms,victim->fd);

                remainder_size = size - nb;
                unlink_chunk(ms, victim);

                /* Exhaust */
                if (remainder_size < MINSIZE) {
                    set_inuse_bit_at_offset(victim, size);
                } else {   /* Split */
                    remainder = chunk_at_offset(victim, nb);

                    /* We cannot assume the unsorted list is empty and therefore
                       have to perform a complete insert here.  */
                    bck = unsorted_chunks(ms);
                    fwd = ptr(ms,bck->fd);
                    remainder->bk = ofs(ms,bck);
                    remainder->fd = ofs(ms,fwd);
                    bck->fd = fwd->bk = ofs(ms,remainder);
                    if (!in_smallbin_range(remainder_size)) {
                        remainder->fd_nextsize = 0;
                        remainder->bk_nextsize = 0;
                    }
                    set_head (victim, nb | PREV_INUSE);
                    set_head (remainder, remainder_size | PREV_INUSE);
                    set_foot (remainder, remainder_size);
                }
                return ofs(ms,chunk2mem(victim));
            }
        }

        /*
         Search for a chunk by scanning bins, starting with next largest
         bin. This search is strictly by best-fit; i.e., the smallest
         (with ties going to approximately the least recently used) chunk
         that fits is selected.
         The bitmap avoids needing to check that most blocks are nonempty.
         The particular case of skipping all bins during warm-up phases
         when no chunks have been returned yet is faster than it might look.
        */

        ++idx;
        bin = bin_at(ms, idx);
        unsigned int block = idx2block(idx);
        unsigned int map = ms->binmap[block];
        unsigned int bit = idx2bit(idx);

        for (;;)
        {
            /* Skip rest of block if there are no more set bits in this block.  */
            if (bit > map || bit == 0) {
                do {
                    if (++block >= BINMAPSIZE) /* out of bins */
                        goto use_top;
                } while ((map = ms->binmap[block]) == 0);
                bin = bin_at(ms, (block << BINMAPSHIFT));
                bit = 1;
            }

            /* Advance to bin with set bit. There must be one. */
            while ((bit & map) == 0) {
                bin = next_bin(bin);
                bit <<= 1;
            }
            /* Inspect the bin. It is likely to be non-empty */
            victim = ptr(ms,last(bin));
            /*  If a false alarm (empty bin), clear the bit. */
            if (victim == bin) {
                ms->binmap[block] = map &= ~bit; /* Write through */
                bin = next_bin(bin);
                bit <<= 1;
            } else {
                size = chunksize(victim);
                /*  We know the first chunk in this bin is big enough to use. */
                remainder_size = size - nb;
                /* unlink */
                unlink_chunk (ms, victim);
                /* Exhaust */
                if (remainder_size < MINSIZE) {
                    set_inuse_bit_at_offset(victim, size);
                } else {   /* Split */
                    remainder = chunk_at_offset(victim, nb);
                    /* We cannot assume the unsorted list is empty and therefore
                       have to perform a complete insert here.  */
                    bck = unsorted_chunks(ms);
                    fwd = ptr(ms,bck->fd);
                    remainder->bk = ofs(ms,bck);
                    remainder->fd = ofs(ms,fwd);
                    bck->fd = fwd->bk = ofs(ms,remainder);

                    /* advertise as last remainder */
                    if (in_smallbin_range(nb))
                        ms->last_remainder = ofs(ms,remainder);
                    if (!in_smallbin_range(remainder_size)) {
                        remainder->fd_nextsize = 0;
                        remainder->bk_nextsize = 0;
                    }
                    set_head (victim, nb | PREV_INUSE);
                    set_head (remainder, remainder_size | PREV_INUSE);
                    set_foot (remainder, remainder_size);
                }
                return ofs(ms,chunk2mem(victim));
            }
        }

    use_top:
      /*
         If large enough, split off the chunk bordering the end of memory
         (held in ms->top). Note that this is in accord with the best-fit
         search rule.  In effect, ms->top is treated as larger (and thus
         less well fitting) than any other available chunk since it can
         be extended to be as large as necessary (up to system
         limitations).
         We require that ms->top always exists (i.e., has size >=
         MINSIZE) after initialization, so if it would otherwise be
         exhausted by current request, it is replenished. (The main
         reason for ensuring it exists is that we may need MINSIZE space
         to put in fenceposts in sysmalloc.)
       */
        victim = ptr(ms,ms->top);
        size = chunksize(victim);

        if ((unsigned long) (size) >= (unsigned long) (nb + MINSIZE)) {
            remainder_size = size - nb;
            remainder = chunk_at_offset(victim, nb);
            ms->top = ofs(ms,remainder);
            set_head(victim, nb | PREV_INUSE);
            set_head(remainder, remainder_size | PREV_INUSE);
            return ofs(ms,chunk2mem(victim));
        } else if (ms->have_fastchunks) {
            malloc_consolidate (ms);
            /* restore original bin index */
            if (in_smallbin_range (nb))
                idx = smallbin_index (nb);
            else
                idx = largebin_index (nb);
        } else {  /* Otherwise, relay to handle system-dependent cases */
            size_t page_size  = getpagesize();
            size_t alloc_size =
                ((nb + MINSIZE - size + page_size - 1) / page_size) * page_size;

            size_t old_size =
                ms->top + size + sizeof(size_t);
            size_t new_size =
                old_size + alloc_size;

            if (fd >= 0) {
                if (ftruncate(fd, new_size) < 0)
                    throw pgf_systemerror(errno, filepath);
            }

// OSX mman and mman-win32 do not implement mremap or MREMAP_MAYMOVE
#ifndef MREMAP_MAYMOVE
            if (munmap(ms, old_size) == -1)
                throw pgf_systemerror(errno);
            malloc_state* new_ms =
                (malloc_state*) mmap(0, new_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
#else
            malloc_state* new_ms =
                (malloc_state*) mremap(ms, old_size, new_size, MREMAP_MAYMOVE);
#endif
            if (new_ms == MAP_FAILED)
                throw pgf_systemerror(errno);

            ms = new_ms;
            current_base = (unsigned char*) ms;

            victim = ptr(ms,ms->top);

            size += alloc_size;

            remainder_size = size - nb;
            remainder = chunk_at_offset(victim, nb);
            ms->top = ofs(ms,remainder);
            set_head(victim, nb | PREV_INUSE);
            set_head(remainder, remainder_size | PREV_INUSE);
            return ofs(ms,chunk2mem(victim));
        }
    }
}

PGF_INTERNAL
void PgfDB::free_internal(object o)
{
    size_t size;                 /* its size */
    object *fb;                  /* associated fastbin */
    mchunk *nextchunk;           /* next contiguous chunk */
    size_t nextsize;             /* its size */
    int nextinuse;               /* true if nextchunk is used */
    size_t prevsize;             /* size of previous contiguous chunk */
    mchunk* bck;                 /* misc temp for linking */
    mchunk* fwd;                 /* misc temp for linking */

    mchunk* p = mem2chunk(ptr(ms,o));
    size = chunksize(p);


    /*
      If eligible, place chunk on a fastbin so it can be found
      and used quickly in malloc.
    */
    if ((unsigned long)(size) <= (unsigned long)(DEFAULT_MXFAST)) {
        ms->have_fastchunks = true;
        unsigned int idx = fastbin_index(size);
        fb = &ms->fastbins[idx];
        /* Atomically link P to its fastbin: P->FD = *FB; *FB = P;  */
        p->fd = *fb;
        *fb = ofs(ms,p);
    } else {     /* Consolidate other chunks as they arrive. */
        nextchunk = chunk_at_offset(p, size);
        nextsize = chunksize(nextchunk);
        /* consolidate backward */
        if (!prev_inuse(p)) {
            prevsize = prev_size(p);
            size += prevsize;
            p = chunk_at_offset(p, -((long) prevsize));
            unlink_chunk (ms, p);
        }
        if (nextchunk != ptr(ms,ms->top)) {
            /* get and clear inuse bit */
            nextinuse = inuse_bit_at_offset(nextchunk, nextsize);
            /* consolidate forward */
            if (!nextinuse) {
                unlink_chunk (ms, nextchunk);
                size += nextsize;
            } else
                clear_inuse_bit_at_offset(nextchunk, 0);
            /*
                Place the chunk in unsorted chunk list. Chunks are
                not placed into regular bins until after they have
                been given one chance to be used in malloc.
            */
            bck = unsorted_chunks(ms);
            fwd = ptr(ms,bck->fd);
            p->fd = ofs(ms,fwd);
            p->bk = ofs(ms,bck);
            if (!in_smallbin_range(size)) {
                p->fd_nextsize = 0;
                p->bk_nextsize = 0;
            }
            bck->fd = ofs(ms,p);
            fwd->bk = ofs(ms,p);
            set_head(p, size | PREV_INUSE);
            set_foot(p, size);
        } else {
            /*
                If the chunk borders the current high end of memory,
                consolidate into top
            */

            size += nextsize;
            set_head(p, size | PREV_INUSE);
            ms->top = ofs(ms,p);
        }

        /*
            If freeing a large space, consolidate possibly-surrounding
            chunks. Then, if the total unused topmost memory exceeds trim
            threshold, ask malloc_trim to reduce top.
            Unless max_fast is 0, we don't know if there are fastbins
            bordering top, so we cannot tell for sure whether threshold
            has been reached unless fastbins are consolidated.  But we
            don't want to consolidate on each free.  As a compromise,
            consolidation is performed if FASTBIN_CONSOLIDATION_THRESHOLD
            is reached.
        */
        if ((unsigned long)(size) >= FASTBIN_CONSOLIDATION_THRESHOLD) {
            if (ms->have_fastchunks)
                malloc_consolidate(ms);
        }
    }
}

PGF_INTERNAL
ref<PgfPGF> PgfDB::revision2pgf(PgfRevision revision)
{
    if (revision <= sizeof(*current_db->ms) || revision >= current_db->ms->top)
        throw pgf_error("Invalid revision");

    mchunk *chunk = mem2chunk(ptr(current_db->ms,revision));
    if (chunksize(chunk) < sizeof(PgfPGF))
        throw pgf_error("Invalid revision");

    ref<PgfPGF> pgf = revision;
    if (chunksize(chunk) != request2size(sizeof(PgfPGF)+pgf->name.size+1))
        throw pgf_error("Invalid revision");

    return pgf;
}

PGF_INTERNAL
bool PgfDB::is_persistant_revision(ref<PgfPGF> pgf)
{
    return (pgf->prev == 0 && pgf->next == 0 &&
            current_db->ms->transient_revisions != pgf);
}

PGF_INTERNAL
void PgfDB::link_transient_revision(ref<PgfPGF> pgf)
{
    pgf->next = current_db->ms->transient_revisions;
    if (current_db->ms->transient_revisions != 0)
        current_db->ms->transient_revisions->prev = pgf;
    current_db->ms->transient_revisions = pgf;
}

PGF_INTERNAL
void PgfDB::unlink_transient_revision(ref<PgfPGF> pgf)
{
    if (pgf->next != 0)
        pgf->next->prev = pgf->prev;
    if (pgf->prev != 0)
        pgf->prev->next = pgf->next;
    else if (current_db->ms->transient_revisions == pgf)
        current_db->ms->transient_revisions = pgf->next;
}

PGF_INTERNAL
void PgfDB::sync()
{
    malloc_state *ms = current_db->ms;
    size_t size =
        ms->top + chunksize(ptr(ms,ms->top)) + sizeof(size_t);

    int res = msync((void *) ms, size, MS_SYNC | MS_INVALIDATE);
    if (res != 0)
        throw pgf_systemerror(errno);
}

DB_scope::DB_scope(PgfDB *db, DB_scope_mode m)
{
    int res =
        (m == READER_SCOPE) ? pthread_rwlock_rdlock(&db->rwlock)
                            : pthread_rwlock_wrlock(&db->rwlock);
    if (res != 0)
        throw pgf_systemerror(res);

    save_db       = current_db;
    current_db    = db;
    current_base  = (unsigned char*) current_db->ms;

    next_scope    = last_db_scope;
    last_db_scope = this;
}

DB_scope::~DB_scope()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wterminate"
    int res = pthread_rwlock_unlock(&current_db->rwlock);
    if (res != 0)
        throw pgf_systemerror(res);

    current_db    = save_db;
    current_base  = current_db ? (unsigned char*) current_db->ms
                               : NULL;

    last_db_scope = next_scope;

#pragma GCC diagnostic pop
}
