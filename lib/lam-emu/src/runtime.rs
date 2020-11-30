use super::coordinator::*;
use super::literal::*;
use super::program::*;
use anyhow::Error;

pub trait SchedulerManager {
    fn setup(&mut self, scheduler_count: u32, program: &Program) -> Result<(), Error>;

    fn run(&self, coordinator: &Coordinator) -> Result<(), Error>;
}

/// This trait represents a runtime system and is used to plug the emulator's side-effects and
/// other functions implemented by the host.
///
pub trait Runtime {
    fn execute(&mut self, call: &MFA, args: &[Literal]) -> Literal;

    fn sleep(&self, _duration: u64) {}

    fn halt(&self) {}

    fn r#yield(&self);
}
