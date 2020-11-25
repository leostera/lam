use anyhow::Error;
use log::{debug, info};
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

use lam_beam::beam_reader;
use lam_compiler::target::Target;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "build",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "build this project"
)]
pub struct BuildOpt {
    #[structopt(
        name = "FILES",
        help = "the .beam files to compile",
        parse(from_os_str)
    )]
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
    target: BuildTarget,

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
pub enum BuildTarget {
    Native,
    WASM,
    Web,
}

impl FromStr for BuildTarget {
    type Err = String;
    fn from_str(target: &str) -> Result<Self, Self::Err> {
        match target {
            "native" => Ok(BuildTarget::Native),
            "wasm" => Ok(BuildTarget::WASM),
            "web" => Ok(BuildTarget::Web),
            _ => Err("Could not parse target. Please use native or wasm".to_string()),
        }
    }
}

impl BuildOpt {
    pub fn build(self) -> Result<(), Error> {
        let t0 = std::time::Instant::now();
        info!("Building project...");

        let t1 = std::time::Instant::now();
        let mut beams = Vec::new();
        for f in self.files {
            beams.push(beam_reader::Reader::from_file(&f)?);
        }
        debug!("Read bytecode in {}ms", t1.elapsed().as_millis());

        let t2 = std::time::Instant::now();
        let program: lam_emu::program::Program = lam_compiler::Translator::default()
            .from_bytecode(beams)
            .with_main(self.entrypoint, "main".to_string());

        debug!("Built program in {}ms", t2.elapsed().as_millis());

        let target = Target::of_program(program).with_name(self.output);

        match self.target {
            BuildTarget::Native => target.to_native(),
            BuildTarget::WASM => target.to_wasm(),
            BuildTarget::Web => target.to_web(),
        }?;

        info!("Done in {}ms", t0.elapsed().as_millis());
        Ok(())
    }
}
