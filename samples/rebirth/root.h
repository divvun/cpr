
#ifndef ROOT_H
#define ROOT_H

#include <dep.h>

#ifdef UNICODE
#define STR wchar_t *
#else
#define STR char *
#endif

MYINT foobar(STR name);

#endif // ROOT_H