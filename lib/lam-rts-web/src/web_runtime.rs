use anyhow::Error;

use wasm_bindgen::prelude::*;
use web_sys::console;

use lam_emu::{List, Literal, Runtime, Scheduler, Value, MFA};
use num_bigint::BigInt;

#[wasm_bindgen]
#[derive(Default, Debug, Clone)]
pub struct WebRuntime {}

impl Runtime for WebRuntime {
    fn execute(&mut self, mfa: &MFA, args: &[Literal]) -> Literal {
        let MFA {
            module,
            function,
            arity: _,
        } = mfa;
        match (module.as_str(), function.as_str()) {
            ("io", "format") => {
                let str = match args[1].clone() {
                    Literal::List(List::Cons(boxed_int, _)) => match *boxed_int {
                        Value::Literal(Literal::Integer(bi)) => bi.to_string(),
                        x => format!("{:?}", x),
                    },
                    x => format!("{:?}", x),
                };
                console::log_1(&str.into());
                Literal::Atom("ok".to_string())
            }
            ("erlang", "-") => {
                let a: BigInt = args[0].clone().into();
                let b: BigInt = args[1].clone().into();
                Literal::Integer(a - b)
            }
            ("erlang", "+") => {
                let a: BigInt = args[0].clone().into();
                let b: BigInt = args[1].clone().into();
                Literal::Integer(a + b)
            }
            (_, _) => panic!("How'd you get here?"),
        }
    }
    fn run_schedulers(&mut self, schedulers: Vec<Scheduler>) -> Result<(), Error> {
        schedulers[0].clone().run(Box::new(self.clone()))
    }

    fn sleep(&self, _delay: u64) {}

    fn halt(&self) -> ! {
        std::process::exit(0)
    }
}
