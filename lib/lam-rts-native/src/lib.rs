use log::debug;

use lam_emu::Coordinator;

mod native_runtime;
use self::native_runtime::*;

/** # Safety
 *  This function will dereference the data pointer to read and then execute the bytecode.
 */
#[export_name = "lam_rts__start"]
pub unsafe extern "C" fn start(data: *const u8, size: usize) {
    let data: &[u8] = std::slice::from_raw_parts(data, size);
    let program = lam_emu::program::Program::deserialize(&data)
        .expect("could not decode program! has this been serialized correctly?");

    env_logger::init();
    debug!("Initializing Native Runtime...");

    let runtime = NativeRuntime::default();
    let scheduler_count = runtime.cpu_count() as u32;
    Coordinator::new(scheduler_count, program, Box::new(runtime))
        .run()
        .unwrap();
}
