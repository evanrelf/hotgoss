mod challenge_1;
mod challenge_2;
mod challenge_3a;
mod challenge_3b;
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
        "hotgoss-2" => challenge_2::main()?,
        "hotgoss-3a" => challenge_3a::main()?,
        "hotgoss-3b" => challenge_3b::main()?,
        binary => anyhow::bail!("wtf does {binary} mean"),
    }

    Ok(())
}
