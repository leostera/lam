#[no_mangle]
pub extern "C" fn start(data: *const u8, size: usize) {
    lam_rts_native::start(data, size)
}
