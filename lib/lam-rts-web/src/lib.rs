use wasm_bindgen::prelude::*;

mod web_runtime;

#[wasm_bindgen]
pub fn start(data: *const u8, size: usize) {
    let program = program(data, size);
    let runtime = web_runtime::WebRuntime::default();
    lam_emu::Emulator::new(program, Box::new(runtime)).run()
}

fn program(data: *const u8, size: usize) -> lam_emu::Program {
    let data: &[u8] = unsafe { std::slice::from_raw_parts(data, size) };
    lam_emu::program::Program::deserialize(&data).unwrap()
}
