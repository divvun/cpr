
// =====================
// Group
// =====================

/* C-style comment */

/*
 * Multi-line C comment
 */

// Multi-line single-\
line comment

// Lots of slashes // More plz // Okay done

#define HELLO "not a /* comment */"
char *not_a_comment = "not /* a comment */";
char *not_a_comment_either = "can't comment // in a string";

typedef const ch\
ar *const_first; // blah
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

#ifdef VOID
int takes_void(void);
int takes_void_ptr(void *a);
#endif // VOID

