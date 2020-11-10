use wasm_bindgen::prelude::*;

mod web_runtime;

#[wasm_bindgen]
pub fn start(data: *const u8, size: usize) {
    let data: &[u8] = unsafe { std::slice::from_raw_parts(data, size) };
    let program = lam_emu::program::Program::deserialize(&data).unwrap();
    let runtime = web_runtime::WebRuntime::default();
    lam_emu::Runner::for_program(program, Box::new(runtime)).run()
}
