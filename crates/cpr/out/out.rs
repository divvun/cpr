
    #![allow(non_camel_case_types)]
    
type const_first = *const i8;
type const_last = *const i8;
type not_const = *mut i8;
type myint = i32;
type mystring = *mut i8;
type myconststring = *const i8;
type a = *mut i32;
type b = *mut *mut i32;
#[repr(C)]
pub struct struct_one {
}
type two = struct_two;
#[repr(C)]
pub struct struct_three {
}
extern "C" {
    pub fn getpid () -> i32;
}
extern "C" {
    pub fn returns_int (__arg0: i32) -> i32;
}
extern "C" {
    pub fn returns_typedef (__arg0: i32) -> mystring;
}
extern "C" {
    pub fn takes_const (which_string: *const i8) -> i32;
}
extern "C" {
    pub fn returns_void ();
}
extern "C" {
    pub fn returns_void_ptr () -> *mut ();
}
extern "C" {
    pub fn takes_void () -> i32;
}
extern "C" {
    pub fn takes_void_ptr (a: *mut ()) -> i32;
}
