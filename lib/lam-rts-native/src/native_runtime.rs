use lam_emu::{Emulator, Runtime, MFA};

#[derive(Default, Debug, Clone)]
pub struct NativeRuntime {}

impl Runtime for NativeRuntime {
    fn execute(&mut self, mfa: &MFA, emu: &mut Emulator) {
        let MFA {
            module,
            function,
            arity,
        } = mfa;
        match (module.as_str(), function.as_str()) {
            ("io", "format") => {
                let mut regs = vec![];
                for i in 0..*arity {
                    regs.push(emu.registers[i as usize].clone());
                }
                match regs[1] {
                    _ => println!("{:?}", regs[0]),
                }
            }
            (_, _) => todo!(),
        }
    }
}
