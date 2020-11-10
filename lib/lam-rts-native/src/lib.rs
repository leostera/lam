mod native_runtime;

#[export_name = "lam_rts__start"]
pub extern "C" fn start(data: *const u8, size: usize) {
    let data: &[u8] = unsafe { std::slice::from_raw_parts(data, size) };
    let program = lam_emu::program::Program::deserialize(&data).unwrap();
    let runtime = native_runtime::NativeRuntime::default();
    lam_emu::Runner::for_program(program, Box::new(runtime)).run()
}
