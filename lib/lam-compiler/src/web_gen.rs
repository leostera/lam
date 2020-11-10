use anyhow::Error;

use super::target::Target;

#[cfg(debug_assertions)]
const RUNTIME: &[u8] =
    include_bytes!("../../../target/wasm32-unknown-unknown/debug/lam_rts_web.wasm");

#[cfg(not(debug_assertions))]
const RUNTIME: &[u8] =
    include_bytes!("../../../target/wasm32-unknown-unknown/release/lam_rts_web.wasm");

impl Target {
    pub fn to_web(&self) -> Result<(), Error> {
        Ok(())
    }
}
