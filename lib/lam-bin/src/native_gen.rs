use anyhow::{Context, Error};
use bincode;
use log::debug;
use std::io::Write;
use std::path::PathBuf;

#[cfg(debug_assertions)]
const RUNTIME: &[u8] = include_bytes!("../../../target/debug/liblamrts.a");

#[cfg(not(debug_assertions))]
const RUNTIME: &[u8] = include_bytes!("../../../target/release/liblamrts.a");

#[derive(Clone, Debug, Default)]
pub struct Target {
    bytecode: lam::program::Program,
    output: PathBuf,
}

impl Target {
    pub fn of_bytecode(bytecode: lam::program::Program) -> Target {
        Target {
            bytecode,
            ..Target::default()
        }
    }

    pub fn with_name(self, output: PathBuf) -> Target {
        Target { output, ..self }
    }

    pub fn compile(self) -> Result<(), Error> {
        debug!("Templating .c file...");

        let bc = self.bytecode;
        let data = bincode::serialize(&bc)?;
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
            &self.output.to_str().context("Bad output path")?,
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
