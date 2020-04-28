
// #include <minwindef.h>
// #include <wintrust.h>
// #include <fileapi.h>
// #include <projectedfslib.h>
// #include <wincrypt.h>

// typedef struct _A { int one; } A, *PA;
// typedef struct _A { int two; } A, *PA;
// typedef struct _A { int two; } A;
// typedef struct _A { int two; } *PA;

typedef struct A {
    int a;
    int b;
    char c;
    unsigned long long big;
} A, *PA;
