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
            arity: _,
        } = mfa;
        match (module.as_str(), function.as_str()) {
            ("io", "format") => {
                println!("{:?}", args);
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
            (_, _) => panic!("How'd you get here? -- {:?} {:?}", mfa, args),
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
