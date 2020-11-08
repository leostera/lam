use fern::colors::{Color, ColoredLevelConfig};
use log::{debug, info};
use std::path::PathBuf;
use structopt::StructOpt;

use lam_cli::bingen;

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
        self.setup_logging();
        self.cmd.run();
    }

    fn setup_logging(&self) {
        let colors_line = ColoredLevelConfig::new()
            .error(Color::Red)
            .warn(Color::Yellow)
            .info(Color::White)
            .debug(Color::White)
            .trace(Color::BrightBlack);
        let colors_level = colors_line.clone().info(Color::Green);
        fern::Dispatch::new()
            .format(move |out, message, record| {
                out.finish(format_args!(
                    "{color_line}{date} {level}{color_line} :: {message}\x1B[0m",
                    color_line = format_args!(
                        "\x1B[{}m",
                        colors_line.get_color(&record.level()).to_fg_str()
                    ),
                    date = chrono::Local::now().format("%H:%M:%S"),
                    level = colors_level.color(record.level()),
                    message = message,
                ));
            })
            .level(if self.verbose {
                log::LevelFilter::Debug
            } else {
                log::LevelFilter::Info
            })
            .level_for("pretty_colored", log::LevelFilter::Trace)
            .chain(std::io::stdout())
            .apply()
            .unwrap();
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Goal {
    Build(BuildOpt),
}

impl Goal {
    fn run(self) {
        match self {
            Goal::Build(opts) => opts.build(),
        }
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
}

impl BuildOpt {
    fn build(self) {
        let t0 = std::time::Instant::now();
        info!("Building project...");
        let bc = vec![
            lam::program::Instruction::Allocate {},
            lam::program::Instruction::Noop,
        ];

        let target = bingen::Target::of_bytecode(bc).with_name(self.output);

        target.compile().unwrap();

        info!("Done in {}ms", t0.elapsed().as_millis());
    }
}

fn main() {
    LAM::from_args().run();
}
