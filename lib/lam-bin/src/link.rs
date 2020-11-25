use anyhow::Error;
use log::{debug, info};
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

use lam_compiler::target::Target;
use lam_emu::Program;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "link",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "link LAM bytecode objects into a binary"
)]
pub struct LinkOpt {
    #[structopt(name = "FILES", help = "the .lam files to compile", parse(from_os_str))]
    files: Vec<PathBuf>,

    #[structopt(
        short = "o",
        long = "output",
        name = "OUTPUT",
        help = "the output path of the executable",
        parse(from_os_str)
    )]
    output: PathBuf,

    #[structopt(
        short = "t",
        long = "target",
        name = "TARGET",
        help = "the target architecture to use: native | wasm | web",
        default_value = "native"
    )]
    target: LinkTarget,

    #[structopt(
        short = "e",
        long = "entrypoint",
        name = "ENTRYPOINT",
        help = "the module where to look for the main function",
        default_value = "main"
    )]
    entrypoint: String,
}

#[derive(StructOpt, Debug, Clone)]
#[structopt()]
pub enum LinkTarget {
    Native,
    WASM,
    Web,
}

impl FromStr for LinkTarget {
    type Err = String;
    fn from_str(target: &str) -> Result<Self, Self::Err> {
        match target {
            "native" => Ok(LinkTarget::Native),
            "wasm" => Ok(LinkTarget::WASM),
            "web" => Ok(LinkTarget::Web),
            _ => Err("Could not parse target. Please use native or wasm".to_string()),
        }
    }
}

impl LinkOpt {
    pub fn link(self) -> Result<(), Error> {
        let t0 = std::time::Instant::now();
        info!("Linking project...");

        // read all the bytecode
        let t1 = std::time::Instant::now();
        let mut programs = Vec::new();
        for f in self.files {
            let bytes = std::fs::read(f)?;
            programs.push(lam_emu::Program::deserialize(&bytes)?);
        }
        debug!("Read bytecode in {}ms", t1.elapsed().as_millis());

        // link it into a single program
        let t2 = std::time::Instant::now();
        let program = Program::link(programs)?.with_main(self.entrypoint, "main".to_string());
        debug!("Built program in {}ms", t2.elapsed().as_millis());

        // compile it to the target
        let t3 = std::time::Instant::now();
        let target = Target::of_program(program).with_name(self.output);
        match self.target {
            LinkTarget::Native => target.to_native(),
            LinkTarget::WASM => target.to_wasm(),
            LinkTarget::Web => target.to_web(),
        }?;
        debug!("Compiled program in {}ms", t3.elapsed().as_millis());

        info!("Done in {}ms", t0.elapsed().as_millis());

        Ok(())
    }
}
