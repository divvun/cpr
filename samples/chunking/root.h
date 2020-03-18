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


