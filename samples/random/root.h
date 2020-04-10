
// typedef short SHORT;
// typedef short int SHORT_INT;
// typedef int INT;
// typedef long LONG;
// typedef long int LONG_INT;
// typedef long long LONG_LONG;
// typedef long long int LONG_LONG_INT;

// typedef unsigned short USHORT;
// typedef unsigned short int USHORT_INT;
// typedef unsigned int UINT;
// typedef unsigned long ULONG;
// typedef unsigned long int ULONG_INT;
// typedef unsigned long long ULONG_LONG;
// typedef unsigned long long int ULONG_LONG_INT;

// typedef signed short SSHORT;
// typedef signed short int SSHORT_INT;
// typedef signed int SINT;
// typedef signed long SLONG;
// typedef signed long int SLONG_INT;
// typedef signed long long SLONG_LONG;
// typedef signed long long int SLONG_LONG_INT;

// typedef enum _EXCEPTION_DISPOSITION {
//   ExceptionContinueExecution = 0,
//   ExceptionContinueSearch = 1,
//   ExceptionNestedException = 2,
//   ExceptionCollidedUnwind = 3,
// } EXCEPTION_DISPOSITION, *PEXCEPTION_DISPOSITION;

// typedef unsigned long long ULLONG;

// typedef struct s {
//     ULLONG a;
//     const struct {
//         char c;
//     } *b;
// } s, *sp;

// enum WOOPS {
//     A = 1 + 0xF00 + 0644,
//     B = (int) 4.0f,
//     C = sizeof(struct s),
//     D = 3 > 2,
//     LAST,
// };

// typedef int INT;
// typedef int INT;

// #include <minwindef.h>

// typedef enum {
//     UNSPECIFIED_COMPARTMENT_ID = 0,
//     DEFAULT_COMPARTMENT_ID
// } COMPARTMENT_ID, *PCOMPARTMENT_ID;

typedef enum {
    UNSPECIFIED_COMPARTMENT_ID = 0,
    DEFAULT_COMPARTMENT_ID
} COMPARTMENT_ID;
