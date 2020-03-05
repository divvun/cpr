
ssize_t seek(int whence, ssize_t off);

#ifdef MODERN_SEEK2
ssize_t seek2(int whence, ssize_t off_modern);
#else
ssize_t seek2(int whence, ssize_t off_legacy);
#endif
