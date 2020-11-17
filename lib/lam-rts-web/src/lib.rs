use wasm_bindgen::prelude::*;

mod web_runtime;
use web_runtime::*;

use lam_emu::{Coordinator, Program};

#[wasm_bindgen]
pub fn start(data: *const u8, size: usize) {
    let program = program(data, size);
    let runtime = WebRuntime::default();
    Coordinator::new(1, program, Box::new(runtime))
        .run()
        .unwrap();
}

fn program(data: *const u8, size: usize) -> lam_emu::Program {
    let data: &[u8] = unsafe { std::slice::from_raw_parts(data, size) };
    Program::deserialize(&data).unwrap()
}
