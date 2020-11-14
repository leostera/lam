use lam_emu::{Literal, Runtime, Value, MFA};

#[derive(Default, Debug, Clone)]
pub struct NativeRuntime {}

impl Runtime for NativeRuntime {
    fn execute(&mut self, mfa: &MFA, args: &[Value]) -> Value {
        let MFA {
            module,
            function,
            arity,
        } = mfa;
        match (module.as_str(), function.as_str()) {
            ("io", "format") => println!("{:?}", args),
            (_, _) => panic!("How'd you get here?"),
        };
        Value::Literal(Literal::Atom("ok".to_string()))
    }
}
