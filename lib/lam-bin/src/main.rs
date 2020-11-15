use log::{debug, info};
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

use lam_beam::beam_reader;
use lam_compiler::target::Target;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "lam",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "
░█▒░▒▄▀▄░█▄▒▄█
▒█▄▄░█▀█░█▒▀▒█

Compile BEAM bytecode to native binaries.
"
)]
struct LAM {
    #[structopt(short = "v", long = "verbose", help = "turn on verbosity")]
    verbose: bool,

    #[structopt(subcommand)]
    cmd: Goal,
}

impl LAM {
    fn run(self) {
        env_logger::init();
        self.cmd.run();
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Goal {
    Build(BuildOpt),
    Dump(DumpOpt),
}

impl Goal {
    fn run(self) {
        match self {
            Goal::Build(opts) => opts.build(),
            Goal::Dump(opts) => opts.dump(),
        }
    }
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "dump", about = "dump the parsed instructions")]
struct DumpOpt {
    #[structopt(
        name = "FILES",
        help = "the .beam files to compile",
        parse(from_os_str)
    )]
    files: Vec<PathBuf>,
}

impl DumpOpt {
    fn dump(self) {
        let t0 = std::time::Instant::now();
        info!("Building project...");

        let t1 = std::time::Instant::now();
        let mut beams = Vec::new();
        for f in self.files {
            beams.push(beam_reader::Reader::from_file(f).unwrap());
        }
        debug!("Read bytecode in {}ms", t1.elapsed().as_millis());

        let t2 = std::time::Instant::now();
        let program: lam_emu::program::Program =
            lam_compiler::Translator::default().from_bytecode(beams);

        debug!("Built program in {}ms", t2.elapsed().as_millis());

        println!("{:#?}", program);

        info!("Done in {}ms", t0.elapsed().as_millis());
    }
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "build", about = "build this project")]
struct BuildOpt {
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
    fn build(self) {
        let t0 = std::time::Instant::now();
        info!("Building project...");

        let t1 = std::time::Instant::now();
        let mut beams = Vec::new();
        for f in self.files {
            beams.push(beam_reader::Reader::from_file(f).unwrap());
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
        }
        .unwrap();

        info!("Done in {}ms", t0.elapsed().as_millis());
    }
}

fn main() {
    LAM::from_args().run();
}
