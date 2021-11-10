#ifndef IPC_H
#define IPC_H

#ifndef _WIN32
#define ipc_rwlock_t pthread_rwlock_t
#define ipc_rwlock_rdlock pthread_rwlock_rdlock
#define ipc_rwlock_wrlock pthread_rwlock_wrlock
#define ipc_rwlock_unlock pthread_rwlock_unlock
#else
typedef struct ipc_rwlock_t ipc_rwlock_t;
int ipc_rwlock_rdlock(ipc_rwlock_t *rwlock);
int ipc_rwlock_wrlock(ipc_rwlock_t *rwlock);
int ipc_rwlock_unlock(ipc_rwlock_t *rwlock);
#endif

PGF_INTERNAL_DECL
ipc_rwlock_t *ipc_new_file_rwlock(const char* file_path,
                                  bool *is_first);

PGF_INTERNAL_DECL
void ipc_release_file_rwlock(const char* file_path,
                             ipc_rwlock_t *rwlock);

#endif
