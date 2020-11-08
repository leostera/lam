use anyhow::{Context, Error};
use log::{debug, info};
use std::path::PathBuf;

#[derive(Clone, Debug, Default)]
pub struct Target {
    bytecode: Vec<lam::program::Instruction>,
    output: PathBuf,
}

impl Target {
    pub fn of_bytecode(bytecode: Vec<lam::program::Instruction>) -> Target {
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
        let bc_size = bc.len() * std::mem::size_of::<lam::program::Instruction>();
        let data = unsafe {
            let bc_ptr = bc.as_ptr() as *const u8;
            std::slice::from_raw_parts(bc_ptr, bc_size)
        };

        let template = include_str!("bin.c").to_string();

        let bc_str = format!("{:?}", data);
        let c_code = template
            .replace("LAM_BYTECODE_SIZE", &bc_size.to_string())
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
            "native.c",
            "./target/debug/liblamrts.a",
            "-lpthread",
            "-ldl",
            "-lm",
        ]);
        let res = cc.status().map(|_| ()).context("Compilation failed")?;

        debug!("Removing temporary files...");

        std::fs::remove_file(path).context("Could not remove .c file")?;

        Ok(res)
    }
}
