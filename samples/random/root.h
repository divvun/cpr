
// struct A {
//     struct {
//         int a;
//         int b;
//     } c;
// };

// struct B {
//     struct {
//         int a;
//         int b;
//     } c;
// };

// typedef int (*CALLBACK)(void *data, int id);

// typedef int (A)(int);

// typedef B C;

#define AS_INT(x) x

// #define SOME_STR "haha"
// #define SOME_CHAR '\n'
// #define N1 0x24
// #define N2 0600
// #define N3 -500
// #define N4 AS_INT(2) + 8
// #define N5 N1 | N2
// #define N6 0xFFFFFFFF

#include <minwindef.h>
#include <fileapi.h>
#include <projectedfslib.h>
