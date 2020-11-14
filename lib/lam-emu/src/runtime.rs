use super::bytecode::*;
use super::program::*;

/** This trait represents a runtime system and is used to plug the emulator's
 *  side-effects and other functions implemented by the host.
 */
pub trait Runtime {
    fn execute(&mut self, call: &MFA, args: &[Value]) -> Value;
}
