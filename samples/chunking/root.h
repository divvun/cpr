
// before

/* 0 REGULAR        */ #if defined(REGULAR)
/* 0 REGULAR        */ struct foo {
/* 0 REGULAR        */     int foo;
/* 1 REGULAR & EVIL */ #if defined(EVIL)
/* 1 REGULAR & EVIL */     int evil;
/* 1 REGULAR & EVIL */ #endif
/* 2 REGULAR        */ }
/* 2 REGULAR        */ #endif

// after

#if defined(REGULAR) && defined(EVIL)
struct foo {
    int foo;
    int evil;
}
#endif

#if defined(REGULAR) && !defined(EVIL)
struct foo {
    int foo;
}
#endif



int foo();

#ifdef NOT_EVIL
int bar();
#endif


//---------------------




