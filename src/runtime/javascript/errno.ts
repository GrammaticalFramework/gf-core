interface ErrorInfo {
  errno: number
  code: string
  description: string
}

// taken from /usr/include/asm-generic/errno-base.h
const errs: ErrorInfo[] = [
  { code: 'EPERM', errno: 1, description: 'Operation not permitted' },
  { code: 'ENOENT', errno: 2, description: 'No such file or directory' },
  { code: 'ESRCH', errno: 3, description: 'No such process' },
  { code: 'EINTR', errno: 4, description: 'Interrupted system call' },
  { code: 'EIO', errno: 5, description: 'I/O error' },
  { code: 'ENXIO', errno: 6, description: 'No such device or address' },
  { code: 'E2BIG', errno: 7, description: 'Argument list too long' },
  { code: 'ENOEXEC', errno: 8, description: 'Exec format error' },
  { code: 'EBADF', errno: 9, description: 'Bad file number' },
  { code: 'ECHILD', errno: 10, description: 'No child processes' },
  { code: 'EAGAIN', errno: 11, description: 'Try again' },
  { code: 'ENOMEM', errno: 12, description: 'Out of memory' },
  { code: 'EACCES', errno: 13, description: 'Permission denied' },
  { code: 'EFAULT', errno: 14, description: 'Bad address' },
  { code: 'ENOTBLK', errno: 15, description: 'Block device required' },
  { code: 'EBUSY', errno: 16, description: 'Device or resource busy' },
  { code: 'EEXIST', errno: 17, description: 'File exists' },
  { code: 'EXDEV', errno: 18, description: 'Cross-device link' },
  { code: 'ENODEV', errno: 19, description: 'No such device' },
  { code: 'ENOTDIR', errno: 20, description: 'Not a directory' },
  { code: 'EISDIR', errno: 21, description: 'Is a directory' },
  { code: 'EINVAL', errno: 22, description: 'Invalid argument' },
  { code: 'ENFILE', errno: 23, description: 'File table overflow' },
  { code: 'EMFILE', errno: 24, description: 'Too many open files' },
  { code: 'ENOTTY', errno: 25, description: 'Not a typewriter' },
  { code: 'ETXTBSY', errno: 26, description: 'Text file busy' },
  { code: 'EFBIG', errno: 27, description: 'File too large' },
  { code: 'ENOSPC', errno: 28, description: 'No space left on device' },
  { code: 'ESPIPE', errno: 29, description: 'Illegal seek' },
  { code: 'EROFS', errno: 30, description: 'Read-only file system' },
  { code: 'EMLINK', errno: 31, description: 'Too many links' },
  { code: 'EPIPE', errno: 32, description: 'Broken pipe' },
  { code: 'EDOM', errno: 33, description: 'Math argument out of domain of func' },
  { code: 'ERANGE', errno: 34, description: 'Math result not representable' }
]

export default {
  lookup (errno: number): string | undefined {
    return errs.find(err => err.errno === errno)?.description
  }
}
