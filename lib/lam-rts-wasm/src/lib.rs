/** # Safety
 *  This function is marked as unsafe because the native runtime will dereference
 *  the data pointer to read and then execute the bytecode.
 */
#[no_mangle]
pub unsafe extern "C" fn start(data: *const u8, size: usize) {
    lam_rts_native::start(data, size)
}
