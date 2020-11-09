#[no_mangle]
pub extern "C" fn start(data: *const u8, size: usize) {
    lamrts::start(data, size)
}
