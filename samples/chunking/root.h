#ifdef FOO
struct foo {
    int foo;
#ifdef EVIL
    int evil;
#else
    int good;
#endif // EVIL
}
#endif // FOO


#ifdef FOO
    struct foo {
        int foo;

    #ifdef EVIL
        int evil;
    #endif

    #if !defined(EVIL)
        int good;
    #endif // EVIL
    }
#endif // FOO

