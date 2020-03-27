
#ifndef ROOT_H
#define ROOT_H

#include "dep.h"

#ifdef UNICODE
#define STR unsigned long int *
#else
#define STR char *
#endif

MYINT foobar(STR name);

#endif // ROOT_H