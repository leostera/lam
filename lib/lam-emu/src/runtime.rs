use super::emulator;
use super::program;

/** This trait represents a runtime system and is used to plug the emulator's
 *  side-effects and other functions implemented by the host.
 */
pub trait Runtime {
    fn execute(&mut self, mfa: &program::MFA, emu: &mut emulator::Emulator);
}
