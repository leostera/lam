use super::literal::*;
use super::program::*;
use anyhow::Error;

/** This trait represents a runtime system and is used to plug the emulator's
 *  side-effects and other functions implemented by the host.
 */
pub trait Runtime {
    fn execute(&mut self, call: &MFA, args: &[Literal]) -> Literal;

    fn run_schedulers(&mut self, scheduler_count: u32, program: &Program) -> Result<(), Error>;

    fn sleep(&self, duration: u64);

    fn halt(&self) -> !;
}
