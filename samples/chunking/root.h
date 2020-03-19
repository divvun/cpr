/* FOO            */ #ifdef FOO     
/* FOO            */ struct foo {
/* FOO            */     int foo;
/* FOO & EVIL     */ #ifdef EVIL
/* FOO & EVIL     */     int evil;
/* FOO & !EVIL    */ #else
/* FOO & !EVIL    */     int good;
/* FOO            */ #endif // EVIL
/* FOO            */ }
/*                */ #endif // FOO

#if defined(FOO)
struct foo {
    int haha;
#if !defined(FOO)
};
#endif // !FOO
#endif // FOO

//  FOO &  (FOO & EVIL) &  (FOO & !EVIL) &  FOO       => false
//  FOO &  (FOO & EVIL) &  (FOO & !EVIL) & !FOO       => false
//  FOO &  (FOO & EVIL) & !(FOO & !EVIL) &  FOO       => FOO & EVIL, parses
//  FOO &  (FOO & EVIL) & !(FOO & !EVIL) & !FOO       => false
//  FOO & !(FOO & EVIL) &  (FOO & !EVIL) &  FOO       => FOO & !EVIL, parses
//  FOO & !(FOO & EVIL) &  (FOO & !EVIL) & !FOO       => false
//  FOO & !(FOO & EVIL) & !(FOO & !EVIL) &  FOO       => false
//  FOO & !(FOO & EVIL) & !(FOO & !EVIL) & !FOO       => false
// !FOO &  (FOO & EVIL) &  (FOO & !EVIL) &  FOO       => false
// !FOO &  (FOO & EVIL) &  (FOO & !EVIL) & !FOO       => false
// !FOO &  (FOO & EVIL) & !(FOO & !EVIL) &  FOO       => false
// !FOO &  (FOO & EVIL) & !(FOO & !EVIL) & !FOO       => false
// !FOO & !(FOO & EVIL) &  (FOO & !EVIL) &  FOO       => false
// !FOO & !(FOO & EVIL) &  (FOO & !EVIL) & !FOO       => false
// !FOO & !(FOO & EVIL) & !(FOO & !EVIL) &  FOO       => false
// !FOO & !(FOO & EVIL) & !(FOO & !EVIL) & !FOO       => !FOO, but zero regions

// ----------------

/* FOO            */ #ifdef FOO     
/* FOO            */ struct foo {
/* FOO            */     int foo;
/* FOO & EVIL     */ #ifdef EVIL
/* FOO & EVIL     */     int evil;
/* FOO            */ #endif // EVIL
/* FOO            */ }
/*                */ #endif // FOO

//  FOO &  (FOO & EVIL) &  FOO   => FOO & EVIL, parses!
//  FOO &  (FOO & EVIL) & !FOO   => false
//  FOO & !(FOO & EVIL) &  FOO   => FOO & !EVIL, parses!
//  FOO & !(FOO & EVIL) & !FOO   => false
// !FOO &  (FOO & EVIL) &  FOO   => false
// !FOO &  (FOO & EVIL) & !FOO   => false
// !FOO & !(FOO & EVIL) &  FOO   => false
// !FOO & !(FOO & EVIL) & !FOO   => !FOO, but zero regions

#if defined(FOO) && defined(EVIL)
struct foo {
    int foo;
    int evil;
}
#endif

#if defined(FOO) && !defined(EVIL)
struct foo {
    int foo;
}
#endif


//----------------

/* 0 FOO               */ #if defined(FOO)
/* 0 FOO               */ struct foo {
/* 1 FOO & BAR         */ #if defined(BAR)
/* 1 FOO & BAR         */     struct {
/* 2 FOO & BAR & BAZ   */ #if defined(BAZ)
/* 2 FOO & BAR & BAZ   */     } baz;
/* 3 FOO & BAR & !BAZ  */ #else
/* 3 FOO & BAR & !BAZ  */     } bar;
/* 4 FOO & BAR         */ #endif // BAZ
/* 5 FOO               */ #endif // BAR
/* 5 FOO               */ };
/* 6 true              */ #endif // FOO

// these regions make up valid C code:

/* 0 FOO               */ #if defined(FOO)
/* 0 FOO               */ struct foo {
/* 2 FOO & BAR & BAZ   */ #if defined(BAZ)
/* 2 FOO & BAR & BAZ   */     } baz;

// ---------

#if defined(FOO)
struct foo {
#if defined(BAR)
    struct {
#if defined(BAZ)
    } baz;
#else
    } bar;
#endif // BAZ
#endif // BAR
};
#endif // FOO
