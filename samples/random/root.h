// #include <minwindef.h>
// #include <wintrust.h>
// #include <fileapi.h>
// #include <projectedfslib.h>
// #include <wincrypt.h>

// typedef struct _A { int one; } A, *PA;
// typedef struct _A { int two; } A, *PA;
// typedef struct _A { int two; } A;
// typedef struct _A { int two; } *PA;

#define UNSIGNED unsigned

#define TWOFIELDS int a; \
    int b;

typedef struct A {
    TWOFIELDS
    char c;
    // woop
    UNSIGNED long lon big;
} A, *PA;
