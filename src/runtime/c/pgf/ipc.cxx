#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>

#include "pgf/data.h"

typedef struct {
    dev_t dev;
    ino_t ino;
    pthread_rwlock_t rwlock;
} file_locks_entry;

typedef struct {
    pthread_mutex_t mutex;
    file_locks_entry entries[];
} file_locks;

static char gf_runtime_locks[] = "/gf-runtime-locks";

PGF_INTERNAL
pthread_rwlock_t *pgf_acquire_file_rwlock(const char* file_path)
{
    int created = 0;

    int fd =
        shm_open(gf_runtime_locks, O_RDWR, 0);
    if (errno == ENOENT) {
        created = 1;
        fd = shm_open(gf_runtime_locks, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
    }
    if (fd < 0) {
        throw pgf_systemerror(errno);
    }

    int pagesize  = getpagesize();
    int n_entries = (pagesize - sizeof(file_locks))
                            / sizeof(file_locks_entry);

    if (ftruncate(fd, pagesize) != 0) {
        throw pgf_systemerror(errno);
    }

    file_locks *locks =
          (file_locks *)
                 mmap(NULL, pagesize,
                      PROT_READ|PROT_WRITE,
                      MAP_SHARED,
                      fd,0);
    if (locks == MAP_FAILED) {
        throw pgf_systemerror(errno);
    }

    if (created) {
        pthread_mutexattr_t attr;
        if (pthread_mutexattr_init(&attr)) {
            throw pgf_systemerror(errno);
        }
        if (pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED)) {
            throw pgf_systemerror(errno);
        }
        if (pthread_mutex_init(&locks->mutex, &attr)) {
            throw pgf_systemerror(errno);
        }

        for (int i = 0; i < n_entries; i++) {
            locks->entries[i].dev = 0;
            locks->entries[i].ino = 0;
        }
    }

    struct stat s;
    if (stat(file_path, &s) != 0) {
        throw pgf_systemerror(errno);
    }

    pthread_mutex_lock(&locks->mutex);

    file_locks_entry *entry = NULL;
    for (int i = 0; i < n_entries; i++) {
        if ((locks->entries[i].dev == s.st_dev &&
             locks->entries[i].ino == s.st_ino) ||
            (locks->entries[i].dev == 0 &&
             locks->entries[i].ino == 0 &&
             entry == NULL)) {
            entry = &locks->entries[i];
        }
    }

    if (entry == NULL) {
        throw pgf_error("Too many files");
    }

    if (entry->dev == 0 && entry->ino == 0) {
        entry->dev = s.st_dev;
        entry->ino = s.st_ino;

        pthread_rwlockattr_t attr;
        if (pthread_rwlockattr_init(&attr) != 0) {
            throw pgf_systemerror(errno);
        }
        if (pthread_rwlockattr_setpshared(&attr, PTHREAD_PROCESS_SHARED) != 0) {
            throw pgf_systemerror(errno);
        }
        if (pthread_rwlock_init(&entry->rwlock, &attr) != 0) {
            throw pgf_systemerror(errno);
        }
    }

    pthread_mutex_unlock(&locks->mutex);

    return &entry->rwlock;
}
