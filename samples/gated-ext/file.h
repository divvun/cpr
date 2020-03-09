
// int open(const char *path);
// int close(int fd);

typedef int myint;
typedef char *mystring;
typedef const char *myconststring;

int returns_int(int a);
mystring returns_typedef(int a);
void returns_void(int a);

int takes_void(void);
int takes_const(const char *which_string);

typedef int *a, **b;

// typedef const char *const_first;
// typedef char const *const_last;
// typedef char *not_const;
