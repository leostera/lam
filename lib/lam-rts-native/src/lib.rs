use log::debug;

use std::env;

use lam_emu::{List, Literal, Value};

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

    lam_emu::Emulator::new(program, Box::new(runtime))
        .preload(0, args())
        .run()
}

pub fn args() -> lam_emu::Value {
    Value::Literal(
        env::args()
            .into_iter()
            .skip(1) // skip the binary name
            .fold(Literal::List(List::Nil), |acc, v| {
                Literal::List(List::Cons(
                    Box::new(Value::Literal(Literal::Binary(v.to_string()))),
                    Box::new(Value::Literal(acc)),
                ))
            }),
    )
}
