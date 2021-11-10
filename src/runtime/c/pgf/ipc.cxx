//#define DEBUG_IPC

#ifdef DEBUG_IPC
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define PGF_INTERNAL static
void ipc_error() {
    perror(NULL);
    exit(1);
}
void ipc_toomany() {
    printf("Too many open grammars");
    exit(1);
}
#else
#include "pgf/data.h"
#define ipc_error() throw pgf_systemerror(errno);
#define ipc_toomany() throw pgf_error("Too many open grammars")
#endif

#ifndef _WIN32
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>

#define ptr_t(x) size_t
#define ptr(o,T)  (o ? (T*) (((uint8_t*) locks) + o) : NULL)
#define offs(p) (((uint8_t*) p) - ((uint8_t*) locks))

typedef struct {
    pid_t pid;
    ptr_t(process_entry) next;
} process_entry;

typedef struct {
    dev_t dev;
    ino_t ino;
    process_entry p;
    ptr_t(lock_entry) next;
    pthread_rwlock_t rwlock;
} lock_entry;

typedef struct {
    pthread_mutex_t mutex;
    ptr_t(lock_entry) lock_entries;
    ptr_t(lock_entry) free_lock_entries;
    ptr_t(process_entry) free_process_entries;
    size_t top;
} file_locks;

static char gf_runtime_locks[] = "/gf-runtime-locks";

static file_locks *locks = NULL;

static
void ipc_cleanup_dead_processes()
{
    ptr_t(lock_entry) *last = &locks->lock_entries;
    lock_entry *entry = ptr(*last, lock_entry);
    while (entry != NULL) {
        process_entry *pentry = &entry->p;
        ptr_t(process_entry) *plast = NULL;
        while (pentry != NULL) {
            char proc_file[32];
            sprintf(proc_file, "/proc/%d", pentry->pid);
            if (access(proc_file, F_OK) != 0) {
                // if there are dead processes -> remove them
                if (plast == NULL) {
                    if (entry->p.next == 0) {
                        *last = entry->next;
                        entry->next  = locks->free_lock_entries;
                        entry->dev   = 0;
                        entry->ino   = 0;
                        entry->p.pid = 0;
                        locks->free_lock_entries = offs(entry);
                        goto next;
                    } else {
                        process_entry *tmp =
                            ptr(pentry->next, process_entry);
                        *pentry   = *tmp;
                        tmp->pid  = 0;
                        tmp->next = locks->free_process_entries;
                        locks->free_process_entries = offs(tmp);
                    }
                } else {
                    *plast = pentry->next;
                    pentry->pid  = 0;
                    pentry->next = locks->free_process_entries;
                    locks->free_process_entries = offs(pentry);
                    pentry = ptr(*plast,process_entry);
                }
            } else {
                plast  = &pentry->next;
                pentry = ptr(*plast,process_entry);
            }
        }

        last  = &entry->next;
next:
        entry = ptr(*last, lock_entry);
    }
}

PGF_INTERNAL
ipc_rwlock_t *ipc_new_file_rwlock(const char* file_path,
                                  bool *is_first)
{
    if (file_path == NULL) {
        *is_first = true;
        pthread_rwlock_t *rwlock = (pthread_rwlock_t *)
            malloc(sizeof(pthread_rwlock_t));
        if (pthread_rwlock_init(rwlock, NULL) != 0) {
            ipc_error();
        }
        return rwlock;
    }

    int pagesize  = getpagesize();

    if (locks == NULL) {
        int created = 0;

        // Uncomment if you want a clean state
        //shm_unlink(gf_runtime_locks);

        int fd =
            shm_open(gf_runtime_locks, O_RDWR, 0);
        if (errno == ENOENT) {
            created = 1;
            fd = shm_open(gf_runtime_locks, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
        }
        if (fd < 0) {
            ipc_error();
        }

        if (ftruncate(fd, pagesize) != 0) {
            close(fd);
            ipc_error();
        }

        locks =
              (file_locks *)
                     mmap(NULL, pagesize,
                          PROT_READ|PROT_WRITE,
                          MAP_SHARED,
                          fd,0);
        close(fd);
        if (locks == MAP_FAILED) {
            locks = NULL;
            ipc_error();
        }

        if (created) {
            pthread_mutexattr_t attr;
            if (pthread_mutexattr_init(&attr)) {
                ipc_error();
            }
            if (pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED)) {
                ipc_error();
            }
            if (pthread_mutex_init(&locks->mutex, &attr)) {
                ipc_error();
            }
            pthread_mutexattr_destroy(&attr);

            locks->lock_entries = 0;
            locks->free_lock_entries = 0;
            locks->free_process_entries = 0;
            locks->top = sizeof(file_locks);
        }
    }

    struct stat s;
    if (stat(file_path, &s) != 0) {
        ipc_error();
    }

    pthread_mutex_lock(&locks->mutex);

    ipc_cleanup_dead_processes();

    lock_entry *entry = ptr(locks->lock_entries, lock_entry);
    while (entry != NULL) {
        if (entry->dev == s.st_dev && entry->ino == s.st_ino) {
            break;
        }
        entry = ptr(entry->next, lock_entry);
    }

    *is_first = false;

    if (entry == NULL) {
        *is_first = true;

        if (locks->free_lock_entries) {
            entry = ptr(locks->free_lock_entries, lock_entry);
            locks->free_lock_entries = entry->next;
        } else {
            if (locks->top + sizeof(lock_entry) > pagesize) {
                pthread_mutex_unlock(&locks->mutex);
                ipc_toomany();
            }
            entry = ptr(locks->top, lock_entry);
            locks->top += sizeof(lock_entry);

            pthread_rwlockattr_t attr;
            if (pthread_rwlockattr_init(&attr) != 0) {
                pthread_mutex_unlock(&locks->mutex);
                ipc_error();
            }
            if (pthread_rwlockattr_setpshared(&attr, PTHREAD_PROCESS_SHARED) != 0) {
                pthread_rwlockattr_destroy(&attr);
                pthread_mutex_unlock(&locks->mutex);
                ipc_error();
            }
            if (pthread_rwlock_init(&entry->rwlock, &attr) != 0) {
                pthread_rwlockattr_destroy(&attr);
                pthread_mutex_unlock(&locks->mutex);
                ipc_error();
            }
            pthread_rwlockattr_destroy(&attr);
        }

        entry->dev   = s.st_dev;
        entry->ino   = s.st_ino;
        entry->p.pid = getpid();
        entry->p.next= 0;
        entry->next  = locks->lock_entries;
        locks->lock_entries = offs(entry);
    } else {
        process_entry *pentry;
        if (locks->free_process_entries) {
            pentry = ptr(locks->free_process_entries,process_entry);
            locks->free_process_entries = pentry->next;
        } else {
            if (locks->top+sizeof(process_entry) > pagesize) {
                pthread_mutex_unlock(&locks->mutex);
                ipc_toomany();
            }
            pentry = ptr(locks->top,process_entry);
            locks->top += sizeof(process_entry);
        }

        pentry->pid  = getpid();
        pentry->next = entry->p.next;
        entry->p.next = offs(pentry);
    }

    pthread_mutex_unlock(&locks->mutex);

    return &entry->rwlock;
}

PGF_INTERNAL
void ipc_release_file_rwlock(const char* file_path,
                             ipc_rwlock_t *rwlock)
{
    if (file_path == NULL) {
        pthread_rwlock_destroy(rwlock);
        free(rwlock);
        return;
    }

    if (locks == NULL)
        return;

    pthread_mutex_lock(&locks->mutex);

    ipc_cleanup_dead_processes();

    lock_entry *entry = ptr(locks->lock_entries,lock_entry);
    ptr_t(lock_entry) *last = &locks->lock_entries;
    while (entry != NULL) {
        if (&entry->rwlock == rwlock) {
            break;
        }
        last  = &entry->next;
        entry = ptr(*last,lock_entry);
    }

    if (entry != NULL) {
        pid_t pid = getpid();
        process_entry *pentry = &entry->p;
        ptr_t(process_entry) *plast = NULL;
        while (pentry != NULL) {
            if (pentry->pid == pid) {
                if (plast == NULL) {
                    if (entry->p.next == 0) {
                        *last = entry->next;
                        entry->next = locks->free_lock_entries;
                        entry->dev = 0;
                        entry->ino = 0;
                        entry->p.pid = 0;
                        locks->free_lock_entries = offs(entry);
                    } else {
                        process_entry *tmp =
                            ptr(pentry->next, process_entry);
                        *pentry = *tmp;
                        tmp->pid  = 0;
                        tmp->next = locks->free_process_entries;
                        locks->free_process_entries = offs(tmp);
                    }
                } else {
                    *plast = pentry->next;
                    pentry->pid  = 0;
                    pentry->next = locks->free_process_entries;
                    locks->free_process_entries = offs(pentry);
                }

                break;
            }

            plast  = &pentry->next;
            pentry = ptr(*plast,process_entry);
        }
    }

    pthread_mutex_unlock(&locks->mutex);
}
#else
PGF_INTERNAL
int ipc_rwlock_rdlock(ipc_rwlock_t *rwlock)
{
    return 0;
}

PGF_INTERNAL
int ipc_rwlock_wrlock(ipc_rwlock_t *rwlock)
{
    return 0;
}

PGF_INTERNAL
int ipc_rwlock_unlock(ipc_rwlock_t *rwlock)
{
    return 0;
}

PGF_INTERNAL
ipc_rwlock_t *ipc_new_file_rwlock(const char* file_path,
                                  bool *is_first)
{
    return NULL;
}

PGF_INTERNAL
void ipc_release_file_rwlock(const char* file_path,
                             ipc_rwlock_t *rwlock)
{
}
#endif

#ifdef DEBUG_IPC
int main(int argc, char *argv[])
{
    if (argc < 3 ||
        (strcmp(argv[1], "r") != 0 && strcmp(argv[1], "w") != 0)) {
        printf("syntax: %s (r|w) <file name>\n", argv[0]);
        return 1;
    }

    pthread_rwlock_t *rwlock = ipc_new_file_rwlock(argv[2]);

    if (strcmp(argv[1],"r") == 0) {
        pthread_rwlock_rdlock(rwlock);
    } else if (strcmp(argv[1],"w") == 0) {
        pthread_rwlock_wrlock(rwlock);
    }

    fputs("> ", stdout);
    fflush(stdout);

    char buf[16];
    read(0, buf, sizeof(buf));

    pthread_rwlock_unlock(rwlock);

    ipc_release_file_rwlock(argv[2], rwlock);

    return 0;
}
#endif
