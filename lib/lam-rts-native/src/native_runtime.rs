use anyhow::Error;
use lam_emu::{
    Coordinator, List, Literal, Program, RunFuel, Runtime, Scheduler, SchedulerManager, Tuple,
    Value, MFA,
};
use log::*;
use num_bigint::BigInt;
use std::env;
use std::str::FromStr;

#[derive(Default, Debug)]
/// The NativeRuntime implements support for schedulers taht should be run natively on any given
/// platform.
///
pub struct NativeSchedulerManager {
    /// The amount of reductions that any single process will run for before being put to sleep. A
    /// higher number here means that at any point in time, any process will be allowed to run for
    /// longer. A smaller number means more intertwined concurrency, but also more context
    /// switches.
    ///
    /// Depending on your system, you may benefit from larger or smaller values here. As always,
    /// measure first.
    reduction_count: u64,
}

impl NativeSchedulerManager {
    pub fn new(reduction_count: u64) -> NativeSchedulerManager {
        NativeSchedulerManager { reduction_count }
    }

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

impl SchedulerManager for NativeSchedulerManager {
    /// For the native runtime (whichever platform that we compile natively to), we'll spin one
    /// thread per scheduler and set them all to run forever.
    ///
    /// In short, the OS will figure out how to preemptively schedule them on each core, so we're
    /// sort of safe on that end.
    ///
    /// Now this also means that our _main_ scheduler must run on a separate thread, so our
    /// coordination loop can run on our main thread.
    fn setup(&mut self, scheduler_count: u32, program: &Program) -> Result<(), Error> {
        let reduction_count = self.reduction_count;
        let args = self.args();

        crossbeam::thread::scope(|scope| {
            for s in 1..=scheduler_count {
                scope.spawn(move |_| {
                    Scheduler::new(s, reduction_count, program.clone())
                        .stepper(RunFuel::Infinite)
                        .step(Box::new(NativeRuntime::new()))
                        .unwrap();
                });
            }

            scope.spawn(|_| {
                Scheduler::new(0, reduction_count, program.clone())
                    .boot(args)
                    .clone()
                    .stepper(RunFuel::Infinite)
                    .step(Box::new(NativeRuntime::new()))
                    .unwrap();
            });
        })
        .unwrap();

        Ok(())
    }

    /// In the native platforms, our main thread will execute this function, and we just want to
    /// kick off the scheduler coordination loop.
    fn run(&self, coordinator: &Coordinator) -> Result<(), Error> {
        loop {
            coordinator.step()?
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct NativeRuntime {}

impl NativeRuntime {
    pub fn new() -> NativeRuntime {
        NativeRuntime::default()
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

    fn sleep(&self, delay: u64) {
        std::thread::sleep(std::time::Duration::from_millis(delay));
    }

    fn halt(&self) -> ! {
        std::process::exit(0)
    }
}
