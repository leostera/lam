use anyhow::Error;
use lam_emu::{List, Literal, Runtime, Scheduler, Value, MFA};
use num_bigint::BigInt;
use std::env;
use std::str::FromStr;

#[derive(Default, Debug, Clone)]
pub struct NativeRuntime {}

impl NativeRuntime {
    pub fn cpu_count(&self) -> usize {
        num_cpus::get()
    }

    pub fn args(&self) -> lam_emu::Value {
        Value::Literal(
            env::args()
                .skip(1) // skip the binary name
                .fold(Literal::List(List::Nil), |acc, v| {
                    Literal::List(List::Cons(
                        Box::new(Value::Literal(Literal::Binary(v))),
                        Box::new(Value::Literal(acc)),
                    ))
                }),
        )
    }
}

impl Runtime for NativeRuntime {
    fn execute(&mut self, mfa: &MFA, args: &[Literal]) -> Literal {
        let MFA {
            module,
            function,
            arity: _,
        } = mfa;
        match (module.as_str(), function.as_str()) {
            ("io", "format") => {
                match args[1].clone() {
                    Literal::List(List::Cons(boxed_int, _)) => match *boxed_int {
                        Value::Literal(Literal::Integer(bi)) => println!("{}", bi.to_string()),
                        x => println!("{:?}", x),
                    },
                    x => println!("{:?}", x),
                }
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
                Literal::Integer(a + &b)
            }
            ("erlang", "list_to_integer") => match args[0].clone() {
                Literal::Binary(str) => Literal::Integer(BigInt::from_str(&str).unwrap()),
                _ => panic!("Could not convert: {:?} to an integer", args[0]),
            },
            (_, _) => panic!("How'd you get here?"),
        }
    }

    fn run_schedulers(&mut self, schedulers: Vec<Scheduler>) -> Result<(), Error> {
        crossbeam::thread::scope(|scope| {
            let mut scopes = vec![];
            for s in schedulers[1..].to_vec() {
                scopes.push(scope.spawn(|_| {
                    s.run(Box::new(self.clone())).unwrap();
                }));
            }
            let mut main_scheduler = schedulers[0].clone();
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
