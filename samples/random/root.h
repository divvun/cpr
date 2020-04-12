
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

typedef int (A)(int);
typedef A (*B);
typedef B C;

// #include <minwindef.h>
// #include <projectedfslib.h>
