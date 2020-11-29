use log::*;
use wasm_bindgen::prelude::*;

mod refs;
mod web_runtime;
use web_runtime::*;

use lam_emu::{Coordinator, List, Literal, Program};

#[wasm_bindgen]
pub fn start(data: *const u8, size: usize) {
    let program = program(data, size);

    let args = Literal::List(List::Nil).into();
    let runtime = WebRuntime::new(args);

    console_log::init_with_level(log::Level::Info).unwrap();
    info!("Initializing Web Runtime...");

    Coordinator::new(1, program, Box::new(runtime))
        .run()
        .unwrap();
}

fn program(data: *const u8, size: usize) -> lam_emu::Program {
    let data: &[u8] = unsafe { std::slice::from_raw_parts(data, size) };
    Program::deserialize(&data).unwrap()
}
