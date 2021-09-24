#ifndef IPC_H
#define IPC_H

PGF_INTERNAL_DECL
pthread_rwlock_t *pgf_acquire_file_rwlock(const char* file_path);

#endif
