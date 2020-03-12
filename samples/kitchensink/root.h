
// =====================
// Group
// =====================

/* C-style comment */

typedef const char *const_first; // blah
typedef char const *const_last;
typedef char *not_const;

// =====================
// Group
// =====================

typedef int myint;
typedef char *mystring;
typedef const char *myconststring;

typedef int *a, **b;

// =====================
// Group
// =====================

struct one {
    int a;
};

typedef struct two {
    int a;
} two;

struct three {
    struct three_inner_named {
        int a;
    } a;
};

int getpid(void);

// // =====================
// // Group
// // =====================

// typedef long long int ohno;

int returns_int(int a);
mystring returns_typedef(int a);
int takes_const(const char *which_string);

void returns_void(void);
void *returns_void_ptr(void);

int takes_void(void);
int takes_void_ptr(void *a);
