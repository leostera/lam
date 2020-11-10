use anyhow::{Context, Error};
use log::debug;
use std::io::Write;

use super::target::Target;

#[cfg(debug_assertions)]
const RUNTIME: &[u8] = include_bytes!("../../../target/debug/liblam_rts_native.a");

#[cfg(not(debug_assertions))]
const RUNTIME: &[u8] = include_bytes!("../../../target/release/liblam_rts_native.a");

impl Target {
    pub fn to_native(self) -> Result<(), Error> {
        debug!("Templating .c file...");

        let data = self.bytecode().serialize()?;
        let bc_str = format!("{:?}", data);

        let runtime_object = std::path::PathBuf::from(&"./liblamrts.a");
        let mut liblamrts_a = std::fs::File::create(&runtime_object)?;
        liblamrts_a.write_all(RUNTIME)?;

        let template = include_str!("bin.c").to_string();
        let c_code = template
            .replace("LAM_BYTECODE_SIZE", &data.len().to_string())
            .replace("LAM_BYTECODE_RAW", &bc_str)
            .replace("{[", "{")
            .replace("]}", "}");
        let path = std::path::PathBuf::from(&"./native.c");
        std::fs::write(&path, c_code).context("Could not write .c file")?;

        debug!("Wrote .c file, calling cc now...");

        let mut cc = std::process::Command::new("cc");
        cc.args(&[
            "-o",
            &self.output().to_str().context("Bad output path")?,
            &path.to_str().context("Bad .c file path")?,
            &runtime_object.to_str().context("Bad runtime object path")?,
            "-lpthread",
            "-ldl",
            "-lm",
        ]);
        let res = cc.status().map(|_| ()).context("Compilation failed")?;

        debug!("Removing temporary files...");

        std::fs::remove_file(runtime_object).context("Could not remove liblamrts.a file")?;
        std::fs::remove_file(path).context("Could not remove .c file")?;

        Ok(res)
    }
}
