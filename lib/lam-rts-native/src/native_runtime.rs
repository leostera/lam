use anyhow::Error;
use lam_emu::{List, Literal, Program, Runtime, Scheduler, Tuple, Value, MFA};
use log::*;
use num_bigint::BigInt;
use std::env;
use std::str::FromStr;

#[derive(Default, Debug, Clone)]
pub struct NativeRuntime {}

impl NativeRuntime {
    pub fn cpu_count(&self) -> usize {
        num_cpus::get()
    }

    pub fn args(&self) -> Value {
        Literal::List(
            env::args()
                .skip(1) // skip the binary name
                .rev()
                .fold(List::Nil, |acc, v| {
                    List::Cons(Box::new(Literal::Binary(v)), Box::new(acc))
                }),
        )
        .into()
    }
}

impl Runtime for NativeRuntime {
    fn execute(&mut self, mfa: &MFA, args: &[Literal]) -> Literal {
        let MFA {
            module,
            function,
            arity,
        } = mfa;
        trace!("{}:{}({:?})", module, function, args.clone());
        match (module.as_str(), function.as_str(), arity) {
            ("binary", "split", 3) => {
                let string: String = args[0].clone().into();
                let at: String = args[1].clone().into();
                let parts: Vec<&str> = string.split(&at).collect();

                Literal::List(parts.iter().rev().fold(List::Nil, |acc, el| {
                    List::Cons(Box::new(Literal::Binary(el.to_string())), Box::new(acc))
                }))
            }
            ("file", "read_file", 1) => {
                let path: String = args[0].clone().into();
                let data = std::fs::read_to_string(&path).unwrap().replace("\n", "");
                Literal::Tuple(Tuple {
                    elements: vec![Literal::Atom("ok".to_string()), Literal::Binary(data)],
                    size: 2,
                })
            }
            ("io", "format", _) => {
                println!("{}", args[1]);
                Literal::Atom("ok".to_string())
            }
            ("erlang", "-", 2) => {
                let a: BigInt = args[0].clone().into();
                let b: BigInt = args[1].clone().into();
                Literal::Integer(a - b)
            }
            ("erlang", "+", 2) => {
                let a: BigInt = args[0].clone().into();
                let b: BigInt = args[1].clone().into();
                Literal::Integer(a + &b)
            }
            ("erlang", "list_to_integer", 1) => match args[0].clone() {
                Literal::Binary(str) => Literal::Integer(BigInt::from_str(&str).unwrap()),
                _ => panic!("Could not convert: {:?} to an integer", args[0]),
            },
            (_, _, _) => panic!("How'd you get here? -- {:?} {:?}", mfa, args),
        }
    }

    fn run_schedulers(&mut self, scheduler_count: u32, program: &Program) -> Result<(), Error> {
        crossbeam::thread::scope(|scope| {
            for s in 1..=scheduler_count {
                let runtime = Box::new(self.clone());
                scope.spawn(move |_| {
                    Scheduler::new(s, &program).run(runtime).unwrap();
                });
            }
            let mut main_scheduler = Scheduler::new(0, program);

            main_scheduler
                .boot(self.args())
                .clone()
                .run(Box::new(self.clone()))
                .unwrap();
        })
        .unwrap();
        Ok(())
    }

    fn sleep(&self, delay: u64) {
        std::thread::sleep(std::time::Duration::from_millis(delay));
    }

    fn halt(&self) -> ! {
        std::process::exit(0)
    }
}
