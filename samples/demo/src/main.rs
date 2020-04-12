const GENERIC_WRITE: u32 = 0x40000000;
const OPEN_EXISTING: u32 = 3;

fn main() {
    bindings::CreateFileA(
        "Cargo.toml\0".as_ptr() as *const i8,
        GENERIC_WRITE,
        0,
        std::ptr::null_mut(),
        OPEN_EXISTING,
        0,
        std::ptr::null_mut(),
    );
}
