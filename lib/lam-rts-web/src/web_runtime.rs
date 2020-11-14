use wasm_bindgen::prelude::*;
use web_sys::console;

use lam_emu::{Literal, Runtime, Value, MFA};

#[wasm_bindgen]
#[derive(Default, Debug, Clone)]
pub struct WebRuntime {}

impl Runtime for WebRuntime {
    fn execute(&mut self, mfa: &MFA, args: &[Value]) -> Value {
        let MFA {
            module,
            function,
            arity,
        } = mfa;
        match (module.as_str(), function.as_str()) {
            ("io", "format") => console::log_1(&format!("{:?}", args).into()),
            (_, _) => panic!("How'd you get here?"),
        };
        Value::Literal(Literal::Atom("ok".to_string()))
    }
}
