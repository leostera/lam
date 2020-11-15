use log::debug;

mod native_runtime;

/** # Safety
 *  This function will dereference the data pointer to read and then execute the bytecode.
 */
#[export_name = "lam_rts__start"]
pub unsafe extern "C" fn start(data: *const u8, size: usize) {
    let data: &[u8] = std::slice::from_raw_parts(data, size);
    let program = lam_emu::program::Program::deserialize(&data)
        .expect("could not decode program! has this been serialized correctly?");
    let runtime = native_runtime::NativeRuntime::default();

    env_logger::init();
    debug!("Initializing Native Runtime...");

    lam_emu::Emulator::new(program, Box::new(runtime)).run()
}
