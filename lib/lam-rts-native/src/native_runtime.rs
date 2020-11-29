use anyhow::Error;
use lam_emu::{List, Literal, Program, RunFuel, Runtime, Scheduler, Tuple, Value, MFA};
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
        trace!("{}:{}({:?})", module, function, args);
        match (module.as_str(), function.as_str(), arity) {
            ("binary", "list_to_bin", 1) => Literal::Binary(args[0].clone().into()),
            ("binary", "split", 3) => {
                let string: String = args[0].clone().into();

                let split_by = args[1].clone();

                let elements: Vec<String> = match split_by {
                    Literal::Binary(str) => vec![str],
                    Literal::List(list) => {
                        let separators: Vec<Literal> = list.into();
                        let separators: Vec<String> =
                            separators.iter().map(|sp| sp.clone().into()).collect();
                        string
                            .split(|c: char| separators.contains(&c.to_string()))
                            .map(|s| s.to_string())
                            .collect()
                    }
                    x => panic!("Cannot call binary:split/3 with a second non-list/non-string parameter: {}", x)
                };

                Literal::List(elements.iter().cloned().rev().fold(List::Nil, |acc, el| {
                    List::Cons(Box::new(Literal::Binary(el)), Box::new(acc))
                }))
            }
            ("file", "read_file", 1) => {
                let path: String = args[0].clone().into();
                let data = std::fs::read_to_string(&path).unwrap();
                Literal::Tuple(Tuple {
                    elements: vec![Literal::Atom("ok".to_string()), Literal::Binary(data)],
                    size: 2,
                })
            }
            ("io", "format", _) => {
                println!("{} {}", args[0], args[1]);
                Literal::Atom("ok".to_string())
            }
            ("erlang", "length", 1) => {
                let list: Vec<Literal> = args[0].clone().into();
                Literal::Integer(list.len().into())
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
        // TODO(@ostera): this should be part of the self struct
        let reduction_count = 1000;

        crossbeam::thread::scope(|scope| {
            for s in 1..=scheduler_count {
                let runtime = Box::new(self.clone());
                scope.spawn(move |_| {
                    Scheduler::new(s, reduction_count, program.clone())
                        .stepper(RunFuel::Infinite, runtime)
                        .step()
                        .unwrap();
                });
            }
            let mut main_scheduler = Scheduler::new(0, reduction_count, program.clone());

            main_scheduler
                .boot(self.args())
                .clone()
                .stepper(RunFuel::Infinite, Box::new(self.clone()))
                .step()
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
