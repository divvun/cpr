
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

// #define AS_INT(x) x

// #define SOME_STR "haha"
// #define SOME_CHAR '\n'
// #define N1 0x24
// #define N2 0600
// #define N3 -500
// #define N4 AS_INT(2) + 8
// #define N5 N1 | N2
// #define N6 0xFFFFFFFF

// #include <minwindef.h>
// #include <fileapi.h>
// #include <projectedfslib.h>


#define XSTATE_LEGACY_FLOATING_POINT        (0)
#define XSTATE_LEGACY_SSE                   (1)
#define XSTATE_GSSE                         (2)
#define XSTATE_AVX                          (XSTATE_GSSE)
#define XSTATE_MPX_BNDREGS                  (3)
#define XSTATE_MPX_BNDCSR                   (4)
#define XSTATE_AVX512_KMASK                 (5)
#define XSTATE_AVX512_ZMM_H                 (6)
#define XSTATE_AVX512_ZMM                   (7)
#define XSTATE_IPT                          (8)
#define XSTATE_CET_U                        (11)
#define XSTATE_LWP                          (62)
#define MAXIMUM_XSTATE_FEATURES             (64)

#define XSTATE_MASK_LEGACY_FLOATING_POINT   (1ui64 << (XSTATE_LEGACY_FLOATING_POINT))
#define XSTATE_MASK_LEGACY_SSE              (1ui64 << (XSTATE_LEGACY_SSE))
#define XSTATE_MASK_LEGACY                  (XSTATE_MASK_LEGACY_FLOATING_POINT | \
                                             XSTATE_MASK_LEGACY_SSE)

#define XSTATE_MASK_GSSE                    (1ui64 << (XSTATE_GSSE))
#define XSTATE_MASK_AVX                     (XSTATE_MASK_GSSE)
#define XSTATE_MASK_MPX                     ((1ui64 << (XSTATE_MPX_BNDREGS)) | \
                                             (1ui64 << (XSTATE_MPX_BNDCSR)))

#define XSTATE_MASK_AVX512                  ((1ui64 << (XSTATE_AVX512_KMASK)) | \
                                             (1ui64 << (XSTATE_AVX512_ZMM_H)) | \
                                             (1ui64 << (XSTATE_AVX512_ZMM)))

#define XSTATE_MASK_IPT                     (1ui64 << (XSTATE_IPT))
#define XSTATE_MASK_CET_U                   (1ui64 << (XSTATE_CET_U))
#define XSTATE_MASK_LWP                     (1ui64 << (XSTATE_LWP))

#define XSTATE_MASK_ALLOWED \
   (XSTATE_MASK_LEGACY | XSTATE_MASK_AVX | XSTATE_MASK_MPX | XSTATE_MASK_AVX512 | \
   XSTATE_MASK_IPT | XSTATE_MASK_CET_U | XSTATE_MASK_LWP)
