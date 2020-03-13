
#[cfg(feature = "REGULAR")]
{
    #[cfg(feature != "EVIL")]    
    {
        struct foo {
            foo: i32,
        }
    }
}

#[cfg(feature = "REGULAR")]
{
    #[cfg(feature != "EVIL")]    
    {
        struct foo {
            foo: i32,
            evil: i32,
        }
    }
}