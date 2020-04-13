use std::{ffi::c_void, os::raw::*, ptr::null_mut};

// const GENERIC_READ: c_int = 0x80000000;
// const OPEN_EXISTING: c_int = 3;

#[allow(non_snake_case)]
fn GENERIC_READ<T: From<u32>>() -> T {
    0x80000000.into()
}

#[allow(non_snake_case)]
fn OPEN_EXISTING<T: From<u32>>() -> T {
    3.into()
}

fn main() {
    unsafe {
        let path: Vec<u16> = "Cargo.toml"
            .encode_utf16()
            .chain(Some(0u16).into_iter())
            .collect();
        let f = bindings::CreateFileW(
            path.as_ptr(),
            GENERIC_READ(),
            0,
            null_mut(),
            OPEN_EXISTING(),
            0,
            null_mut(),
        );

        let mut buffer = [0u8; 256];
        let mut read_bytes = 0;
        let success = bindings::ReadFile(
            f,
            buffer.as_mut_ptr() as *mut c_void,
            buffer.len() as c_uint,
            &mut read_bytes,
            null_mut(),
        );
        if success == 0 {
            panic!("ReadFile call failed");
        }
        println!(
            "{}",
            String::from_utf8_lossy(&buffer[..read_bytes as usize])
        );
    }
}
