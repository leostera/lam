use wasm_bindgen::prelude::*;
use web_sys::console;

use lam_emu::{Emulator, Runtime, MFA};

#[wasm_bindgen]
#[derive(Default, Debug, Clone)]
pub struct WebRuntime {}

impl Runtime for WebRuntime {
    fn execute(&mut self, mfa: &MFA, emu: &mut Emulator) {
        let MFA {
            module,
            function,
            arity,
        } = mfa;
        match (module.as_str(), function.as_str()) {
            ("io", "format") => {
                let mut regs = vec![];
                for i in 0..*arity {
                    regs.push(emu.registers[i as usize].clone());
                }
                match regs[1] {
                    _ => console::log_1(&format!("{:?}", regs[0]).into()),
                }
            }
            (_, _) => todo!(),
        }
    }
}
