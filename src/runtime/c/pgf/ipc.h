#ifndef IPC_H
#define IPC_H

PGF_INTERNAL_DECL
pthread_rwlock_t *ipc_new_file_rwlock(const char* file_path);

PGF_INTERNAL_DECL
void ipc_release_file_rwlock(const char* file_path,
                             pthread_rwlock_t *rwlock);

#endif
