
#ifdef UNICODE
#define STRING_TYPE wchar_t*
#else
#define STRING_TYPE char*
#endif // UNICODE

int strlen(STRING_TYPE s);

// =====================

#if defined(UNICODE)
int strlen(wchar_t* s);
#endif

#if !defined(UNICODE)
int strlen(char* s);
#endif
