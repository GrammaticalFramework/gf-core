#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include "data.h"

#ifndef _WIN32

#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>

#else

static
size_t getpagesize()
{
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwPageSize;
}

#define ftruncate _chsize
#define getpid GetCurrentProcessId

static
int last_error_to_errno()
{
    switch (GetLastError()) {
    case ERROR_SUCCESS:
        return 0;
    case ERROR_OUTOFMEMORY:
        return ENOMEM;
    case ERROR_HANDLE_DISK_FULL:
        return ENOSPC;
    default:
        return EINVAL;
    }
}
#endif

PGF_INTERNAL __thread unsigned char* current_base __attribute__((tls_model("initial-exec"))) = NULL;
PGF_INTERNAL __thread PgfDB* current_db __attribute__((tls_model("initial-exec"))) = NULL;
PGF_INTERNAL __thread DB_scope *last_db_scope __attribute__((tls_model("initial-exec"))) = NULL;


#define ptr(T,o) ((T*) (base + (o)))

/* pad request bytes into a usable size -- internal version */
#define request2size(req)                                         \
   (((req) + MALLOC_ALIGN_MASK) & ~MALLOC_ALIGN_MASK)

static char slovo[5] = {'S','L','O','V','O'};

typedef struct {
#ifndef _WIN32
    pid_t pid;
#else
    DWORD pid;
#endif
    object o;
    txn_t txn_id;
    size_t ref_count;
} revision_entry;

struct PGF_INTERNAL_DECL block_descr
{
    size_t sz;            // the size of the binary tree
    size_t block_size;    // the size of the block
    object o;             // the block itself
    object left;
    object right;
    object chain;         // links descriptors that are not in use
    txn_t  block_txn_id;  // transaction which released the block
    txn_t  descr_txn_id;  // transaction which allocated the descriptor
};

struct PGF_INTERNAL_DECL malloc_state
{
    /* Each .ngf file starts with 'SLOVO' as in:
     *    "V načaloto be slovoto" (In the beginning was the word)
     * In this way we detect an attempt to read a non .ngf file.
     */
    char sign[5];

    /* The current size is used to detect when the file was resized
     * by another process. The file size doesn't include the first 4K */
    size_t file_size;

#ifndef _WIN32
    pthread_mutex_t rev_mutex;
    pthread_mutex_t write_mutex;
    pthread_rwlock_t rwlock;
#else
    /* Stores a Reader/Writer lock for Windows */
    LONG rwlock;
#endif

    txn_t curr_txn_id;
    txn_t min_txn_id;

    /* The top address that is not allocated yet. */
    object top;    
    object free_blocks;  // a binary tree of descriptors for free blocks
    object free_descriptors;

    size_t n_revisions;
    object active_revision;
    revision_entry revisions[];
};


PGF_INTERNAL
PgfDB::PgfDB(const char* filepath, int flags, int mode) {
    bool is_new = false;

    fd = -1;
    ms = NULL;
    ref_count = 0;
    page_size = getpagesize();
    pid = getpid();

    if (filepath == NULL) {
        this->filepath = NULL;
        mmap_size = page_size*2;
        is_new    = true;
    } else {
        fd = open(filepath, flags, mode);
        if (fd < 0)
            throw pgf_systemerror(errno, filepath);

        mmap_size = lseek(fd, 0, SEEK_END);
        if (mmap_size == ((off_t) -1)) {
            int code = errno;
            close(fd);
            throw pgf_systemerror(code, filepath);
        }

        is_new = false;
        if (mmap_size == 0) {
            mmap_size = page_size*2;
            if (ftruncate(fd, mmap_size) < 0) {
                int code = errno;
                close(fd);
                throw pgf_systemerror(code, filepath);
            }
            is_new = true;
        }

        this->filepath = strdup(filepath);
        if (this->filepath == NULL) {
            close(fd);
            throw pgf_systemerror(ENOMEM);
        }
    }

    int code = 0;
#ifndef _WIN32
#ifndef MREMAP_MAYMOVE
    if (fd >= 0) {
        ms = (malloc_state*)
            mmap(NULL, mmap_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
        if (ms == MAP_FAILED) {
            code = errno;
            ms = NULL; // mark that ms is not created.
            base = NULL;
            ::free((void *) this->filepath);
            close(fd);
            throw pgf_systemerror(code, filepath);
        }

        mmap_size -= page_size;
        base = ((unsigned char *) ms) + page_size;
    } else {
        ms = (malloc_state*) ::malloc(page_size);
        if (ms == NULL)
            throw pgf_systemerror(ENOMEM);

        mmap_size -= page_size;

        base = (unsigned char *) ::malloc(mmap_size);
        if (base == NULL)
            throw pgf_systemerror(ENOMEM);
    }
#else
    int mflags = (fd < 0) ? (MAP_PRIVATE | MAP_ANONYMOUS) : MAP_SHARED;
    ms = (malloc_state*)
        mmap(NULL, mmap_size, PROT_READ | PROT_WRITE, mflags, fd, 0);
    if (ms == MAP_FAILED) {
        code = errno;
        ms = NULL; // mark that ms is not created.
        base = NULL;
        ::free((void *) this->filepath);
        close(fd);
        throw pgf_systemerror(code, filepath);
    }

    mmap_size -= page_size;
    base = ((unsigned char *) ms) + page_size;
#endif
#else
    char *rev_mutex_name;
    char *write_mutex_name;
    char *event_name;
    char buf[256];

    if (fd >= 0) {
        BY_HANDLE_FILE_INFORMATION hInfo;
        if (!GetFileInformationByHandle((HANDLE) _get_osfhandle(fd), &hInfo)) {
            code = last_error_to_errno();
            ::free((void *) this->filepath);
            close(fd);
            throw pgf_systemerror(code);
        }
        rev_mutex_name = buf;
        sprintf(rev_mutex_name,
                     "gf-rev-mutex-%lx-%lx-%lx",
                     hInfo.dwVolumeSerialNumber,
                     hInfo.nFileIndexHigh,
                     hInfo.nFileIndexLow);
        write_mutex_name = rev_mutex_name+strlen(rev_mutex_name+1);
        sprintf(write_mutex_name,
                     "gf-write-mutex-%lx-%lx-%lx",
                     hInfo.dwVolumeSerialNumber,
                     hInfo.nFileIndexHigh,
                     hInfo.nFileIndexLow);
        event_name = write_mutex_name+strlen(write_mutex_name+1);
        sprintf(event_name,
                     "gf-rwevent-%lx-%lx-%lx",
                     hInfo.dwVolumeSerialNumber,
                     hInfo.nFileIndexHigh,
                     hInfo.nFileIndexLow);

        hMap = CreateFileMapping((HANDLE) _get_osfhandle(fd),
                                 NULL,
                                 PAGE_READWRITE,
                                 HIWORD(mmap_size), LOWORD(mmap_size),
                                 NULL);
        if (hMap != NULL) {
            ms = (malloc_state*) MapViewOfFile(hMap,
                                               FILE_MAP_WRITE,
                                               0,0,mmap_size);
            if (ms == NULL) {
                code = last_error_to_errno();
                CloseHandle(hMap);
                ::free((void *) this->filepath);
                close(fd);
                throw pgf_systemerror(code, filepath);
            }
            mmap_size -= page_size;
            base = ((unsigned char *) ms) + page_size;
        } else {
            code = last_error_to_errno();
            ::free((void *) this->filepath);
            close(fd);
            throw pgf_systemerror(code, filepath);
        }
    } else {
        rev_mutex_name = NULL;
        write_mutex_name = NULL;
        event_name = NULL;
        hMap = INVALID_HANDLE_VALUE;
        ms   = (malloc_state*) ::malloc(mmap_size);
        if (ms == NULL)
            throw pgf_systemerror(ENOMEM);
        mmap_size -= page_size;
        base = ((unsigned char *) ms) + page_size;
    }

    hRevMutex = CreateMutex(NULL, FALSE, rev_mutex_name);
    if (hRevMutex == NULL) {
        code = last_error_to_errno();
        if (fd < 0) {
            ::free(ms);
        } else {
            UnmapViewOfFile(ms);
            CloseHandle(hMap);
        }
        ::free((void *) this->filepath);
        close(fd);
        throw pgf_systemerror(code, filepath);
    }

    hWriteMutex = CreateMutex(NULL, FALSE, write_mutex_name);
    if (hWriteMutex == NULL) {
        CloseHandle(hRevMutex);
        code = last_error_to_errno();
        if (fd < 0) {
            ::free(ms);
        } else {
            UnmapViewOfFile(ms);
            CloseHandle(hMap);
        }
        ::free((void *) this->filepath);
        close(fd);
        throw pgf_systemerror(code, filepath);
    }

    hRWEvent = CreateEvent(NULL, FALSE, FALSE, event_name);
    if (hRWEvent == NULL) {
        CloseHandle(hWriteMutex);
        CloseHandle(hRevMutex);
        if (fd < 0) {
            ::free(ms);
        } else {
            UnmapViewOfFile(ms);
            CloseHandle(hMap);
        }
        ::free((void *) this->filepath);
        close(fd);
        throw pgf_systemerror(code, filepath);
    }
#endif

    if (is_new) {
        code = init_state();
        if (code != 0) {
#ifndef _WIN32
#ifndef MREMAP_MAYMOVE
            if (fd < 0) {
              ::free(ms);
              ::free(base);
            } else
#endif
            munmap(ms,page_size+mmap_size);
#else
            if (fd < 0) {
                ::free(ms);
            } else {
                UnmapViewOfFile(ms);
                CloseHandle(hMap);
            }
            CloseHandle(hRWEvent);
            CloseHandle(hWriteMutex);
            CloseHandle(hRevMutex);
#endif

            ::free((void *) this->filepath);
            close(fd);

            throw pgf_systemerror(code, filepath);
        }
    } else {
        if (strncmp(ms->sign, slovo, sizeof(ms->sign)) != 0) {
#ifndef _WIN32
#ifndef MREMAP_MAYMOVE
            if (fd < 0) {
              ::free(ms);
              ::free(base);
            } else
#endif
            munmap(ms,page_size+mmap_size);
#else
            if (fd < 0) {
                ::free(ms);
            } else {
                UnmapViewOfFile(ms);
                CloseHandle(hMap);
            }
            CloseHandle(hRWEvent);
            CloseHandle(hWriteMutex);
            CloseHandle(hRevMutex);
#endif

            ::free((void *) this->filepath);
            close(fd);
            throw pgf_error("Invalid file content");
        }

        cleanup_revisions();
    }

    top                 = ms->top;
    free_blocks         = ms->free_blocks;
    free_descriptors[0] = ms->free_descriptors;
    free_descriptors[1] = 0;
    free_descriptors[2] = 0;
    last_free_block     = 0;
    last_free_block_size= 0;
}

PGF_INTERNAL
PgfDB::~PgfDB()
{
#ifndef _WIN32
#ifndef MREMAP_MAYMOVE
    if (fd < 0) {
        pthread_rwlock_destroy(&ms->rwlock);
        pthread_mutex_destroy(&ms->write_mutex);
        pthread_mutex_destroy(&ms->rev_mutex);
        ::free(ms);
        ::free(base);
    } else
#endif
    {
        if (ms != NULL)
            munmap(ms,sizeof(*ms));
        if (base != NULL)
            munmap(base,mmap_size);
    }
#else
    if (fd < 0) {
        ::free(ms);
    } else {
        UnmapViewOfFile(ms);
        CloseHandle(hMap);
    }
    CloseHandle(hRWEvent);
    CloseHandle(hWriteMutex);
    CloseHandle(hRevMutex);
#endif

    if (fd >= 0)
        close(fd);

    ::free((void*) filepath);
}

PGF_INTERNAL
txn_t PgfDB::get_txn_id() {
    return current_db->ms->curr_txn_id;
}

PGF_INTERNAL
object PgfDB::register_revision(object o, txn_t txn_id)
{
#ifndef _WIN32
    pthread_mutex_lock(&ms->rev_mutex);
#else
    WaitForSingleObject(hRevMutex, INFINITE);
#endif

    bool found = false;
    revision_entry *found_entry = NULL;

#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, "revisions");
#endif

    ms->min_txn_id = SIZE_MAX;
    for (size_t i = 0; i < ms->n_revisions; i++) {
        revision_entry *entry = &ms->revisions[i];
        if (entry->ref_count == 0) {
            if (found_entry == NULL)
                found_entry = entry;
        } else {
#ifdef DEBUG_MEMORY_ALLOCATOR
            fprintf(stderr, " %ld:%s(%016lx):%ld",
                            entry->txn_id,
                            ((entry->o & MALLOC_ALIGN_MASK) == PgfPGF::tag) ? "pgf" : "concr",
                            entry->o & ~MALLOC_ALIGN_MASK,
                            entry->ref_count);
#endif

            if (entry->pid == pid &&
                entry->o   == o) {
                entry->ref_count++;
                entry->txn_id = txn_id;
                found_entry = entry;
                found = true;
            }

            if (ms->min_txn_id > entry->txn_id)
                ms->min_txn_id = entry->txn_id;
        }
    }

    if (!found) {
        if (found_entry == NULL) {
            size_t n_max = (page_size-sizeof(malloc_state))/sizeof(revision_entry);
            if (ms->n_revisions >= n_max) {
#ifndef _WIN32
                pthread_mutex_unlock(&ms->rev_mutex);
#else
                ReleaseMutex(hRevMutex);
#endif
                throw pgf_error("Too many retained database revisions");
            }
            found_entry = &ms->revisions[ms->n_revisions++];
        }

        found_entry->pid = pid;
        found_entry->o   = o;
        found_entry->ref_count = 1;
        found_entry->txn_id = txn_id;
    }

#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, " minimal %ld\n", ms->min_txn_id);
#endif

#ifndef _WIN32
    pthread_mutex_unlock(&ms->rev_mutex);
#else
    ReleaseMutex(hRevMutex);
#endif

    return (found_entry - ms->revisions) + 1;
}

PGF_INTERNAL
void PgfDB::unregister_revision(object revision)
{
    if (revision == 0 || revision-1 >= ms->n_revisions)
        throw pgf_error("Invalid revision");

    revision_entry *entry = &ms->revisions[revision-1];
    if (entry->ref_count == 0)
        throw pgf_error("Invalid revision");

#ifndef _WIN32
    pthread_mutex_lock(&ms->rev_mutex);
#else
    WaitForSingleObject(hRevMutex, INFINITE);
#endif

    if (--entry->ref_count == 0) {
        // Maybe this was the last revision in the list.
        // Decrement n_revisions if possible.
        while (ms->revisions[ms->n_revisions-1].ref_count == 0){
            ms->n_revisions--;
        }
    }

#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, "revisions");
#endif

    ms->min_txn_id = SIZE_MAX;
    for (size_t i = 0; i < ms->n_revisions; i++) {
        revision_entry *entry = &ms->revisions[i];
        if (entry->ref_count > 0) {
#ifdef DEBUG_MEMORY_ALLOCATOR
            fprintf(stderr, " %ld:%s(%016lx):%ld",
                            entry->txn_id,
                            ((entry->o & MALLOC_ALIGN_MASK) == PgfPGF::tag) ? "pgf" : "concr",
                            entry->o & ~MALLOC_ALIGN_MASK,
                            entry->ref_count);
#endif

            if (ms->min_txn_id > entry->txn_id)
                ms->min_txn_id = entry->txn_id;
        }
    }

#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, " minimal %ld\n", ms->min_txn_id);
#endif

#ifndef _WIN32
    pthread_mutex_unlock(&ms->rev_mutex);
#else
    ReleaseMutex(hRevMutex);
#endif
}

void PgfDB::cleanup_revisions()
{
#ifndef _WIN32
    pthread_mutex_lock(&ms->rev_mutex);
#else
    WaitForSingleObject(hRevMutex, INFINITE);
#endif

#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, "revisions");
#endif

    ms->min_txn_id = SIZE_MAX;
    // If there are dead processes, set their reference counts to 0.
    for (size_t i = 0; i < ms->n_revisions; i++) {
        revision_entry *entry = &ms->revisions[i];
        if (entry->ref_count > 0) {
#ifndef _WIN32
            char proc_file[32];
            sprintf(proc_file, "/proc/%d", entry->pid);
            bool alive = (access(proc_file, F_OK) == 0);
#else
            HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION,
                                          FALSE,entry->pid);
            DWORD dwExitCode = STILL_ACTIVE;
            if (hProcess != NULL)
                GetExitCodeProcess(hProcess,&dwExitCode);
            bool alive = (dwExitCode == STILL_ACTIVE);
            CloseHandle(hProcess);
#endif

            if (alive) {
#ifdef DEBUG_MEMORY_ALLOCATOR
                fprintf(stderr, " %ld:%s(%016lx):%ld",
                                entry->txn_id,
                                ((entry->o & MALLOC_ALIGN_MASK) == PgfPGF::tag) ? "pgf" : "concr",
                                entry->o & ~MALLOC_ALIGN_MASK,
                                entry->ref_count);
#endif
                if (ms->min_txn_id > entry->txn_id)
                    ms->min_txn_id = entry->txn_id;
            } else {
                entry->ref_count = 0;
            }
        }
    }

#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, " minimal %ld\n", ms->min_txn_id);
#endif

#ifndef _WIN32
    pthread_mutex_unlock(&ms->rev_mutex);
#else
    ReleaseMutex(hRevMutex);
#endif
}

PGF_INTERNAL
object PgfDB::get_active_revision()
{
    return current_db->ms->active_revision;
}

PGF_INTERNAL
int PgfDB::init_state()
{
    memcpy(ms->sign, slovo, sizeof(ms->sign));

    ms->top = request2size(1);  // we don't want to start from 0
    ms->file_size = mmap_size;

#ifdef _WIN32
    ms->rwlock = 0;
#else
    int res;

    {
        pthread_mutexattr_t attr;
        if ((res = pthread_mutexattr_init(&attr)) != 0) {
            return res;
        }
        if ((res = pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED)) != 0) {
            return res;
        }
        if ((res = pthread_mutex_init(&ms->rev_mutex, &attr)) != 0) {
            pthread_mutexattr_destroy(&attr);
            return res;
        }
        if ((res = pthread_mutex_init(&ms->write_mutex, &attr)) != 0) {
            pthread_mutexattr_destroy(&attr);
            return res;
        }
        pthread_mutexattr_destroy(&attr);
    }

    {
        pthread_rwlockattr_t attr;
        if ((res = pthread_rwlockattr_init(&attr)) != 0) {
            return res;
        }
        if (fd >= 0 &&
            (res = pthread_rwlockattr_setpshared(&attr, PTHREAD_PROCESS_SHARED)) != 0) {
            pthread_rwlockattr_destroy(&attr);
            return res;
        }
        if ((res = pthread_rwlock_init(&ms->rwlock, &attr)) != 0) {
            pthread_rwlockattr_destroy(&attr);
            return res;
        }
        pthread_rwlockattr_destroy(&attr);
    }
#endif

    ms->curr_txn_id = 1;
    ms->min_txn_id  = 0;
    ms->free_blocks = 0;
    ms->free_descriptors = 0;
    ms->n_revisions = 0;
    return 0;
}

PGF_INTERNAL
size_t PgfDB::block_descr_size(object map)
{
    if (map == 0)
        return 0;
    return ptr(block_descr, map)->sz;
}

PGF_INTERNAL_DECL object PgfDB::new_block_descr(object o, size_t size, txn_t txn_id)
{
    object odescr;
    block_descr *descr;

    if (free_descriptors[0] != 0) {
        odescr = free_descriptors[0];
        descr  = ptr(block_descr, odescr);
        free_descriptors[0] = descr->chain;

#ifdef DEBUG_MEMORY_ALLOCATOR
        fprintf(stderr, "recycled block descriptor %016lx\n", odescr);
#endif
    } else {
        size_t block_size = request2size(sizeof(block_descr));

        size_t free_size = mmap_size - top;
        if (block_size > free_size) {
            size_t alloc_size =
                ((block_size - free_size + page_size - 1) / page_size) * page_size;
            size_t new_size =
                ms->file_size + alloc_size;

            resize_map(new_size);
        }

        odescr = top;
        top += block_size;

#ifdef DEBUG_MEMORY_ALLOCATOR
        fprintf(stderr, "allocated new block descriptor %016lx\n", odescr);
#endif

        descr  = ptr(block_descr, odescr);
    }

    descr->sz     = 1;
    descr->left   = 0;
    descr->right  = 0;
    descr->chain  = 0;
    descr->o      = o;
    descr->block_size = size;
    descr->block_txn_id = txn_id;
    descr->descr_txn_id = ms->curr_txn_id;

    return odescr;
}

PGF_INTERNAL
object PgfDB::upd_block_descr(object map, object left, object right)
{
    block_descr *descr = ptr(block_descr, map);

    if (descr->descr_txn_id != ms->curr_txn_id) {
        block_descr *new_descr;

        descr->chain = free_descriptors[1];
        free_descriptors[1] = map;
        if (free_descriptors[2] == 0)
            free_descriptors[2] = map;

        if (free_descriptors[0] != 0) {
            map = free_descriptors[0];
            new_descr = ptr(block_descr, map);
            free_descriptors[0] = new_descr->chain;

#ifdef DEBUG_MEMORY_ALLOCATOR
            fprintf(stderr, "recycled block descriptor %016lx\n", map);
#endif
        } else {
            size_t block_size = request2size(sizeof(block_descr));

            size_t free_size = mmap_size - top;
            if (block_size > free_size) {
                size_t alloc_size =
                    ((block_size - free_size + page_size - 1) / page_size) * page_size;
                size_t new_size =
                    ms->file_size + alloc_size;

                resize_map(new_size);

                // refresh the pointer
                descr = ptr(block_descr, map);
            }

            map = top;
            top += block_size;

#ifdef DEBUG_MEMORY_ALLOCATOR
            fprintf(stderr, "allocated new block descriptor %016lx\n", map);
#endif

            new_descr = ptr(block_descr, map);
        }

        new_descr->block_size = descr->block_size;
        new_descr->o      = descr->o;
        new_descr->chain  = 0;
        new_descr->block_txn_id = descr->block_txn_id;
        new_descr->descr_txn_id = ms->curr_txn_id;

        descr = new_descr;
    }

    descr->sz    = 1+block_descr_size(left)+block_descr_size(right);
    descr->left  = left;
    descr->right = right;

    return map;
}

PGF_INTERNAL
object PgfDB::balanceL_block_descriptor(object map)
{
    block_descr *descr = ptr(block_descr, map);
    if (descr->right == 0) {
        if (descr->left == 0) {
            return map;
        } else {
            block_descr *left = ptr(block_descr, descr->left);
            if (left->left == 0) {
                if (left->right == 0) {
                    return map;
                } else {
                    object left_right = left->right;
                    object left  = upd_block_descr(descr->left,0,0);
                    object right = upd_block_descr(map,0,0);
                    return upd_block_descr(left_right,
                                           left,
                                           right);
                }
            } else {
                if (left->right == 0) {
                    object left_left = left->left;
                    object left  = descr->left;
                    object right = upd_block_descr(map,0,0);
                    return upd_block_descr(left,
                                           left_left,
                                           right);
                } else {
                    object left_left  = left->left;
                    object left_right = left->right;
                    if (ptr(block_descr, left_right)->sz < 2 * ptr(block_descr, left_left)->sz) {
                        object left  = descr->left;
                        object right =
                            upd_block_descr(map,
                                            left_right,
                                            0);
                        return upd_block_descr(left,
                                               left_left,
                                               right);
                    } else {
                        object left  =
                            upd_block_descr(descr->left,
                                            left_left,
                                            ptr(block_descr, left_right)->left);
                        object right =
                            upd_block_descr(map,
                                            ptr(block_descr, left_right)->right,
                                            0);
                        return upd_block_descr(left_right,
                                               left,
                                               right);
                    }
                }
            }
        }
    } else {
        if (descr->left == 0) {
            return map;
        } else {
            if (ptr(block_descr, descr->left)->sz > 3*ptr(block_descr, descr->right)->sz) {
                block_descr *left = ptr(block_descr, descr->left);
                object left_left  = left->left;
                object left_right = left->right;

                if (ptr(block_descr, left_right)->sz < 2*ptr(block_descr, left_left)->sz) {
                    object left  = descr->left;
                    object right =
                        upd_block_descr(map,
                                        left_right,
                                        descr->right);
                    return upd_block_descr(left,
                                           left_left,
                                           right);
                } else {
                    object left =
                        upd_block_descr(descr->left,
                                        left_left,
                                        ptr(block_descr, left_right)->left);
                    object right =
                        upd_block_descr(map,
                                        ptr(block_descr, left_right)->right,
                                        ptr(block_descr, map)->right);
                    return upd_block_descr(left_right,
                                           left,
                                           right);
                }
            } else {
                return map;
            }
        }
    }
}

PGF_INTERNAL
object PgfDB::balanceR_block_descriptor(object map)
{
    block_descr *descr = ptr(block_descr, map);
    if (descr->left == 0) {
        if (descr->right == 0) {
            return map;
        } else {
            block_descr *right = ptr(block_descr, descr->right);
            if (right->left == 0) {
                if (right->right == 0) {
                    return map;
                } else {
                    object right_right = right->right;
                    object right = descr->right;
                    object left  =
                        upd_block_descr(map,
                                        0,
                                        0);
                    return upd_block_descr(right,
                                           left,
                                           right_right);
                }
            } else {
                if (right->right == 0) {
                    object right_left = right->left;
                    object right =
                        upd_block_descr(descr->right,0,0);
                    object left =
                        upd_block_descr(map,0,0);
                    return upd_block_descr(right_left,
                                           left,
                                           right);
                } else {
                    object right_left  = right->left;
                    object right_right = right->right;
                    if (ptr(block_descr,right_left)->sz < 2 * ptr(block_descr,right_right)->sz) {
                        object right = descr->right;
                        object left  =
                            upd_block_descr(map,
                                            0,
                                            right_left);
                        return upd_block_descr(right,
                                               left,
                                               right_right);
                    } else {
                        object right =
                            upd_block_descr(descr->right,
                                            ptr(block_descr,right_left)->right,
                                            right_right);
                        object left =
                            upd_block_descr(map,
                                            0,
                                            ptr(block_descr,right_left)->left);
                        return upd_block_descr(right_left,
                                               left,
                                               right);
                    }
                }
            }
        }
    } else {
        if (descr->right == 0) {
            return map;
        } else {
            if (ptr(block_descr,descr->right)->sz > 3*ptr(block_descr,descr->left)->sz) {
                block_descr *right = ptr(block_descr,descr->right);
                object right_left  = right->left;
                object right_right = right->right;
                if (ptr(block_descr,right_left)->sz < 2*ptr(block_descr,right_right)->sz) {
                    object right = descr->right;
                    object left =
                        upd_block_descr(map,
                                        descr->left,
                                        right_left);
                    return upd_block_descr(right,
                                           left,
                                           right_right);
                } else {
                    object right =
                        upd_block_descr(descr->right,
                                        ptr(block_descr,right_left)->right,
                                        right_right);
                    object left =
                        upd_block_descr(map,
                                        descr->left,
                                        ptr(block_descr,right_left)->left);
                    return upd_block_descr(right_left,
                                           left,
                                           right);
                }
            } else {
                return map;
            }
        }
    }
}

PGF_INTERNAL
object PgfDB::pop_first_block_descriptor(object map, object *res)
{
    block_descr *descr = ptr(block_descr, map);
    if (map == 0) {
        return 0;
    } else if (descr->left == 0) {
        *res = map;
        return descr->right;
    } else {
        object right = descr->right;
        object left  = pop_first_block_descriptor(descr->left, res);
        map = upd_block_descr(map, left, right);
        return balanceR_block_descriptor(map);
    }
}

PGF_INTERNAL
object PgfDB::pop_last_block_descriptor(object map, object *res)
{
    block_descr *descr = ptr(block_descr, map);
    if (map == 0) {
        return 0;
    } else if (descr->right == 0) {
        *res = map;
        return descr->left;
    } else {
        object left  = descr->left;
        object right = pop_last_block_descriptor(descr->right, res);
        map = upd_block_descr(map, left, right);
        return balanceL_block_descriptor(map);
    }
}

PGF_INTERNAL
object PgfDB::delete_block_descriptor(object map, size_t *psize, object *po)
{
    if (map == 0) {
        *po = 0;
        return 0;
    }

    block_descr *descr = ptr(block_descr, map);
    int cmp = (*psize < descr->block_size) ? -1
            : (*psize > descr->block_size) ? 1
            : (descr->block_txn_id > ms->min_txn_id) ? -1
            : 0;
    if (cmp < 0) {
        object right = descr->right;
        object left  = delete_block_descriptor(descr->left, psize, po);
        if (*po != 0) {
            map = upd_block_descr(map,left,right);
            map = balanceR_block_descriptor(map);
        } else {
            descr = ptr(block_descr, map);
            if (descr->block_txn_id < ms->min_txn_id) {
                *psize = descr->block_size;
                goto fit;
            }
        }
        return map;
    } else if (cmp > 0) {
        object left  = descr->left;
        object right = delete_block_descriptor(descr->right, psize, po);
        if (*po != 0) {
            map = upd_block_descr(map,left,right);
            map = balanceL_block_descriptor(map);
        }
        return map;
    } else {
fit:
        *po = descr->o;

        object new_map;
        if (descr->left == 0) {
            new_map = descr->right;
        } else if (descr->right == 0) {
            new_map = descr->left;
        } else {
            if (ptr(block_descr,descr->left)->sz > ptr(block_descr,descr->right)->sz) {
                object right = descr->right;
                object left  = pop_last_block_descriptor(descr->left, &new_map);
                new_map = upd_block_descr(new_map, left, right);
                new_map = balanceR_block_descriptor(new_map);
            } else {
                object left  = descr->left;
                object right = pop_first_block_descriptor(descr->right, &new_map);
                new_map = upd_block_descr(new_map, left, right);
                new_map = balanceL_block_descriptor(new_map);
            }
        }

        int index = (descr->descr_txn_id != ms->curr_txn_id);
        descr->chain = free_descriptors[index];
        free_descriptors[index] = map;
        if (index == 1 && free_descriptors[2] == 0)
            free_descriptors[2] = map;

        return new_map;
    }
}

#ifdef DEBUG_MEMORY_ALLOCATOR
PGF_INTERNAL void PgfDB::dump_free_blocks(object map)
{
    if (map == 0) {
        fprintf(stderr, ".");
        return;
    }

    block_descr *descr = ptr(block_descr, map);
    if (descr->left != 0 || descr->right != 0) {
        fprintf(stderr, "(");
        dump_free_blocks(descr->left);
        fprintf(stderr, " ");
    }

    fprintf(stderr, "[%016lx %ld %ld]", descr->o, descr->block_size, descr->block_txn_id);

    if (descr->left != 0 || descr->right != 0) {
        fprintf(stderr, " ");
        dump_free_blocks(descr->right);
        fprintf(stderr, ")");
    }
}
#endif

PGF_INTERNAL
object PgfDB::malloc_internal(size_t bytes)
{
#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, "malloc_internal(%ld)\n", bytes);
#endif

    size_t block_size = request2size(bytes);

    object o;
    size_t alloc_size = block_size;
    free_blocks = delete_block_descriptor(free_blocks, &alloc_size, &o);
    if (o != 0) {
        if (alloc_size > block_size) {
            free_blocks = insert_block_descriptor(free_blocks, o+block_size, alloc_size-block_size);
        }

#ifdef DEBUG_MEMORY_ALLOCATOR
        dump_free_blocks(free_blocks);
        fprintf(stderr, "\n");
        fprintf(stderr, "recycled %016lx\n", o);
#endif

        return o;
    } else {
#ifdef DEBUG_MEMORY_ALLOCATOR
        fprintf(stderr, "allocated from top %016lx\n", top);
#endif
    }

    size_t free_size = mmap_size - top;
    if (block_size > free_size) {
        size_t alloc_size =
            ((block_size - free_size + page_size - 1) / page_size) * page_size;
        size_t new_size =
            ms->file_size + alloc_size;

        resize_map(new_size);
    }

    o = top;
    top += block_size;

    return o;
}

PGF_INTERNAL
object PgfDB::realloc_internal(object oldo, size_t old_bytes, size_t new_bytes, txn_t txn_id)
{
    if (oldo == 0)
        return malloc_internal(new_bytes);

    size_t old_nb = request2size(old_bytes);
    size_t new_nb = request2size(new_bytes);

    if (txn_id == ms->curr_txn_id) {
        if (old_nb == new_nb)
            return oldo;

        if (oldo + old_nb == top) {
            ssize_t nb        = new_nb-old_nb;
            ssize_t free_size = mmap_size - top;

            if (nb > free_size) {
                size_t alloc_size =
                    ((nb - free_size + page_size - 1) / page_size) * page_size;
                size_t new_size =
                    ms->file_size + alloc_size;

                resize_map(new_size);
            }

            // If the object is at the end of the allocation area
            top += nb;

#ifdef DEBUG_MEMORY_ALLOCATOR
            fprintf(stderr, "realloc_internal(%016lx,%ld,%ld)\n", oldo, old_bytes, new_bytes);
#endif

            return oldo;
        }
    }

    object newo = malloc_internal(new_bytes);
    memcpy(base+newo, base+oldo, (old_bytes < new_bytes) ? old_bytes : new_bytes);
    free_internal(oldo, old_bytes);
    return newo;
}

PGF_INTERNAL
object PgfDB::insert_block_descriptor(object map, object o, size_t size)
{
    txn_t txn_id = (o >= ms->top) ? 0 : ms->curr_txn_id;

    if (map == 0)
        return new_block_descr(o, size, txn_id);

    block_descr *descr = ptr(block_descr, map);
    int cmp = (size < descr->block_size) ? -1
            : (size > descr->block_size) ? 1
            : (txn_id < descr->block_txn_id) ? -1
            : (txn_id > descr->block_txn_id) ? 1
            : ptr(block_descr,descr->left)->sz < ptr(block_descr,descr->right)->sz ? -1
            : 1;
    if (cmp < 0) {
        object right = descr->right;
        object left  = insert_block_descriptor(descr->left, o, size);
        map = upd_block_descr(map,left,right);
        return balanceL_block_descriptor(map);
    } else {
        object left  = descr->left;
        object right = insert_block_descriptor(descr->right, o, size);
        map = upd_block_descr(map,left,right);
        return balanceR_block_descriptor(map);
    }
}

PGF_INTERNAL
void PgfDB::free_internal(object o, size_t bytes)
{
#ifdef DEBUG_MEMORY_ALLOCATOR
    fprintf(stderr, "free_internal(%016lx,%ld)\n", o, bytes);
#endif

    if (o == 0)
        return;

    size_t block_size = request2size(bytes);

    if (o >= ms->top && o+block_size == top) {
        // The block has been allocated in the current transaction
        // and it is the last just before the top area. We can
        // simply decrement top;
        top -= block_size;
        return;
    }

    if (last_free_block != 0) {
        if (last_free_block+last_free_block_size == o) {
            last_free_block_size += block_size;
#ifdef DEBUG_MEMORY_ALLOCATOR
            fprintf(stderr, "merged block %016lx %ld\n", last_free_block, last_free_block_size);
#endif
        } else if (o+block_size == last_free_block) {
            last_free_block = o;
            last_free_block_size += block_size;
#ifdef DEBUG_MEMORY_ALLOCATOR
            fprintf(stderr, "merged block %016lx %ld\n", last_free_block, last_free_block_size);
#endif
        }
    }

    last_free_block = o;
    last_free_block_size = block_size;

    free_blocks = insert_block_descriptor(free_blocks, o, block_size);

#ifdef DEBUG_MEMORY_ALLOCATOR
    dump_free_blocks(free_blocks);
    fprintf(stderr, "\n");
#endif
}

PGF_INTERNAL
ref<PgfPGF> PgfDB::revision2pgf(PgfRevision revision, size_t *p_txn_id)
{
    if (revision == 0 || revision-1 >= ms->n_revisions)
        throw pgf_error("Invalid revision");

    revision_entry *entry = &ms->revisions[revision-1];
    if (entry->ref_count == 0)
        throw pgf_error("Invalid revision");

    if (ref<PgfPGF>::get_tag(entry->o) != PgfPGF::tag)
        throw pgf_error("Invalid revision");

    ref<PgfPGF> pgf = ref<PgfPGF>::untagged(entry->o);
    if (pgf.as_object() >= top)
        throw pgf_error("Invalid revision");

    if (p_txn_id != NULL)
        *p_txn_id = entry->txn_id;

    return pgf;
}

PGF_INTERNAL
ref<PgfConcr> PgfDB::revision2concr(PgfConcrRevision revision, size_t *p_txn_id)
{
    if (revision == 0 || revision-1 >= ms->n_revisions)
        throw pgf_error("Invalid revision");

    revision_entry *entry = &ms->revisions[revision-1];
    if (entry->ref_count == 0)
        throw pgf_error("Invalid revision");

    if (ref<PgfPGF>::get_tag(entry->o) != PgfConcr::tag)
        throw pgf_error("Invalid revision");

    ref<PgfConcr> concr = ref<PgfConcr>::untagged(entry->o);
    if (concr.as_object() >= top)
        throw pgf_error("Invalid revision");

    if (p_txn_id != NULL)
        *p_txn_id = entry->txn_id;

    return concr;
}

PGF_INTERNAL
void PgfDB::start_transaction()
{
#ifndef _WIN32
    pthread_mutex_lock(&ms->write_mutex);
#else
    WaitForSingleObject(hWriteMutex, INFINITE);
#endif

    top = ms->top;
    free_blocks = ms->free_blocks;
    free_descriptors[0] = ms->free_descriptors;
    free_descriptors[1] = 0;
    free_descriptors[2] = 0;
    last_free_block      = 0;
    last_free_block_size = 0;
}

PGF_INTERNAL
void PgfDB::commit(object o)
{
    if (last_free_block != 0) {
        last_free_block      = 0;
        last_free_block_size = 0;
    }

    malloc_state *ms = current_db->ms;
    object save_top = ms->top;
    object save_free_blocks = ms->free_blocks;
    object save_free_descriptors = ms->free_descriptors;
    object save_active_revision = ms->active_revision;

    int res;
#ifndef _WIN32
#ifndef MREMAP_MAYMOVE
    if (current_db->fd < 0) {
        ms->active_revision = o;
        ms->top = top;
        ms->free_blocks = free_blocks;
        if (free_descriptors[2] != 0) {
            ptr(block_descr,free_descriptors[2])->chain = free_descriptors[0];
            free_descriptors[0] = free_descriptors[1];
            free_descriptors[1] = 0;
            free_descriptors[2] = 0;
        }
        ms->free_descriptors = free_descriptors[0];
        ms->curr_txn_id++;
        res = 0;
    } else {
#endif
        if (free_descriptors[2] != 0) {
            ptr(block_descr,free_descriptors[2])->chain = free_descriptors[0];
            free_descriptors[0] = free_descriptors[1];
            free_descriptors[1] = 0;
            free_descriptors[2] = 0;
        }

        res = msync((void *) base, mmap_size, MS_SYNC | MS_INVALIDATE);
        if (res != 0)
            throw pgf_systemerror(errno);

        ms->active_revision = o;
        ms->top = top;
        ms->free_blocks = free_blocks;
        ms->free_descriptors = free_descriptors[0];
        ms->curr_txn_id++;

        res = msync((void *) ms, page_size, MS_SYNC | MS_INVALIDATE);
        if (res != 0) {
            ms->active_revision = save_active_revision;
            ms->top = save_top;
            ms->free_blocks = save_free_blocks;
            ms->free_descriptors = save_free_descriptors;
            ms->curr_txn_id--;
            throw pgf_systemerror(errno);
        }
#ifndef MREMAP_MAYMOVE
    }
#endif

    pthread_mutex_unlock(&ms->write_mutex);
#else
    if (current_db->fd > 0) {
        if (free_descriptors[2] != 0) {
            ptr(block_descr,free_descriptors[2])->chain = free_descriptors[0];
            free_descriptors[0] = free_descriptors[1];
            free_descriptors[1] = 0;
            free_descriptors[2] = 0;
        }

        if (!FlushViewOfFile(base,mmap_size)) {
            throw pgf_systemerror(last_error_to_errno());
        }
        ms->active_revision = o;
        ms->top = top;
        ms->free_blocks = free_blocks;
        ms->free_descriptors = free_descriptors[0];
        ms->curr_txn_id++;
        if (!FlushViewOfFile(ms,page_size)) {
            ms->active_revision = save_active_revision;
            ms->top = save_top;
            ms->free_blocks = save_free_blocks;
            ms->free_descriptors = save_free_descriptors;
            ms->curr_txn_id--;
            throw pgf_systemerror(last_error_to_errno());
        }
    }

    ReleaseMutex(hWriteMutex);
#endif
}

PGF_INTERNAL
void PgfDB::rollback()
{
    top = ms->top;
    free_blocks = ms->free_blocks;
    free_descriptors[0] = ms->free_descriptors;
    free_descriptors[1] = 0;
    free_descriptors[2] = 0;
    last_free_block      = 0;
    last_free_block_size = 0;

#ifndef _WIN32
    pthread_mutex_unlock(&ms->write_mutex);
#else
    ReleaseMutex(hWriteMutex);
#endif
}

#ifdef _WIN32
#define MAX_SPIN 50000

__forceinline __int16 ReaderCount(unsigned __int32 lock)
{
	return lock & 0x00007FFF;
}

__forceinline __int32 SetReaders(unsigned __int32 lock, unsigned __int16 readers)
{
	return (lock & ~0x00007FFF) | readers;
}

__forceinline __int16 WaitingCount(unsigned __int32 lock)
{
	return (__int16) ((lock & 0x3FFF8000) >> 15);
}

__forceinline __int32 SetWaiting(unsigned __int32 lock, unsigned __int16 waiting)
{
	return (lock & ~0x3FFF8000) | (waiting << 15);
}

__forceinline bool Writer(unsigned __int32 lock)
{
	return (lock & 0x40000000) != 0;
}

__forceinline __int32 SetWriter(unsigned __int32 lock, bool writer)
{
	if(writer)
		return lock | 0x40000000;
	else
		return lock & ~0x40000000;
}

__forceinline bool AllClear(unsigned __int32 lock)
{
	return (lock & 0x40007FFF) == 0;
}
#endif

void PgfDB::lock(DB_scope_mode m)
{
    if (m == READER_SCOPE) {
#ifndef _WIN32
        int res = pthread_rwlock_rdlock(&ms->rwlock);
        if (res != 0)
            throw pgf_systemerror(res);
#else
        for (int i = 0; ; ++i) {
            unsigned __int32 temp = ms->rwlock;
            if (!Writer(temp)) {
                if (InterlockedCompareExchange(&ms->rwlock, SetReaders(temp, ReaderCount(temp) + 1), temp) == temp)
                    return;
                else
                    continue;
            } else {
                if (i < MAX_SPIN) {
                    YieldProcessor();
                    continue;
                }

                //The pending write operation is taking too long, so we'll drop to the kernel and wait
                if (InterlockedCompareExchange(&ms->rwlock, SetWaiting(temp, WaitingCount(temp) + 1), temp) != temp)
                    continue;

                i = 0; //Reset the spincount for the next time
                WaitForSingleObject(hRWEvent, INFINITE);

                do
                {
                    temp = ms->rwlock;
                } while (InterlockedCompareExchange(&ms->rwlock, SetWaiting(temp, WaitingCount(temp) - 1), temp) != temp);
            }
        }
#endif
    }

	// If another process has resized the file we must resize the map
	if (mmap_size != ms->file_size)
		resize_map(ms->file_size);
}

void PgfDB::unlock()
{
#ifndef _WIN32
    pthread_rwlock_unlock(&ms->rwlock);
#else
    while (true) {
        unsigned __int32 temp = ms->rwlock;
        if (ReaderCount(temp) > 0) {
            if (ReaderCount(temp) == 1 && WaitingCount(temp) != 0) {
                //Note: this isn't nor has to be thread-safe, as the worst a duplicate notification can do
                //is cause a waiting to reader to wake, perform a spinlock, then go back to sleep

                //We're the last reader and there's a pending write
                //Wake one waiting writer
                SetEvent(hRWEvent);
            }

            //Decrement reader count
            if (InterlockedCompareExchange(&ms->rwlock, SetReaders(temp, ReaderCount(temp) - 1), temp) == temp)
                break;
        } else {
        }
    }
#endif
}

void PgfDB::resize_map(size_t new_size)
{
#ifndef _WIN32
	int res;
    unsigned char* new_base;
    
    if ((res = pthread_rwlock_wrlock(&ms->rwlock)) != 0)
        throw pgf_systemerror(res);

// OSX does not implement mremap or MREMAP_MAYMOVE
#ifndef MREMAP_MAYMOVE
	if (fd >= 0) {
		if (munmap(base, mmap_size) == -1)
			throw pgf_systemerror(errno);
		base = NULL;
		if (ms->file_size != new_size) {
			if (ftruncate(fd, page_size+new_size) < 0)
				throw pgf_systemerror(errno, filepath);
		}
		new_base =
			(unsigned char *) mmap(0, new_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, page_size);
		if (new_base == MAP_FAILED)
			throw pgf_systemerror(errno);
	} else {
		new_base = (unsigned char *) ::realloc(base, new_size);
		if (new_base == NULL)
			throw pgf_systemerror(ENOMEM);
	}
#else
	if (fd >= 0 && ms->file_size != new_size) {
		if (ftruncate(fd, page_size+new_size) < 0)
			throw pgf_systemerror(errno, filepath);
	}
	new_base =
		(unsigned char *) mremap(base, mmap_size, new_size, MREMAP_MAYMOVE);
	if (new_base == MAP_FAILED)
		throw pgf_systemerror(errno);
#endif

    base = new_base;
#else

    for (int i = 0; ; ++i) {
        unsigned __int32 temp = ms->rwlock;
        if (AllClear(temp)) {
            if (InterlockedCompareExchange(&ms->rwlock, SetWriter(temp, true), temp) == temp)
                return;
            else
                continue;
        } else {
            if (i < MAX_SPIN) {
                YieldProcessor();
                continue;
            }

            //The pending write operation is taking too long, so we'll drop to the kernel and wait
            if (InterlockedCompareExchange(&ms->rwlock, SetWaiting(temp, WaitingCount(temp) + 1), temp) != temp)
                continue;

            i = 0; //Reset the spincount for the next time
            WaitForSingleObject(hRWEvent, INFINITE);

            do
            {
                temp = ms->rwlock;
            } while (InterlockedCompareExchange(&ms->rwlock, SetWaiting(temp, WaitingCount(temp) - 1), temp) != temp);
        }
    }

    new_size += page_size;
	if (fd >= 0) {
		UnmapViewOfFile(ms);
		CloseHandle(hMap);
		ms = NULL;
        base = NULL;

		hMap = CreateFileMapping((HANDLE) _get_osfhandle(fd),
								 NULL,
								 PAGE_READWRITE,
								 HIWORD(new_size), LOWORD(new_size),
								 NULL);
		if (hMap == NULL) {
			hMap = INVALID_HANDLE_VALUE;
			throw pgf_systemerror(last_error_to_errno());
		}

		ms = (malloc_state*) MapViewOfFile(hMap,
										   FILE_MAP_WRITE,
                                           0,0,new_size);
		if (ms == NULL)
			throw pgf_systemerror(last_error_to_errno());
	} else {
		malloc_state *new_ms = (malloc_state*) ::realloc(ms, new_size);
		if (new_ms == NULL)
			throw pgf_systemerror(ENOMEM);
        ms = new_ms;
	}
    new_size -= page_size;

    base = ((unsigned char*) ms) + page_size;
#endif
    current_base = (unsigned char*) base;
	mmap_size = new_size;
	ms->file_size = new_size;
    

#ifndef _WIN32
    if ((res = pthread_rwlock_unlock(&ms->rwlock)) != 0)
        throw pgf_systemerror(res);
#else
    while(true) {
        LONG temp;
        while(true) {
            temp = ms->rwlock;
            assert(Writer(temp));
            if (WaitingCount(temp) == 0)
                break;

            //Note: this is thread-safe (there's guaranteed not to be another EndWrite simultaneously)
            //Wake all waiting readers or writers, loop until wake confirmation is received
            SetEvent(hRWEvent);
        }

        //Decrement writer count
        if (InterlockedCompareExchange(&ms->rwlock, SetWriter(temp, false), temp) == temp)
            break;
    }
#endif
}

DB_scope::DB_scope(PgfDB *db, DB_scope_mode m)
{
    db->lock(m);

    mode          = m;
    save_db       = current_db;
    current_db    = db;
    current_base  = db->base;

    next_scope    = last_db_scope;
    last_db_scope = this;
}

DB_scope::~DB_scope()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wterminate"
    if (mode == READER_SCOPE)
        current_db->unlock();

    current_db    = save_db;
    current_base  = current_db ? current_db->base
                               : NULL;

    last_db_scope = next_scope;
#pragma GCC diagnostic pop
}
