mod challenge_1;
mod protocol;

use anyhow::Context as _;
use clap::Parser as _;
use std::path::PathBuf;

#[derive(clap::Parser)]
#[clap(no_binary_name = true)]
struct Args {
    #[clap(hide = true)]
    binary: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let binary = args
        .binary
        .file_name()
        .context("Failed to get binary name")?
        .to_str()
        .context("Binary name not valid UTF-8")?;

    match binary {
        "hotgoss-1" => challenge_1::main()?,
        binary => anyhow::bail!("wtf does {binary} mean"),
    }

    Ok(())
}