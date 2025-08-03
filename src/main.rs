// src/main.rs

use cscript::analyzer::Analyzer;
use cscript::codegen;
use cscript::diagnostics::DiagnosticBag;
use cscript::parser;
use clap::Parser;
use std::fs;
use std::path::Path;
use std::process::{self, Command};

/// A C-like language compiler with an LLVM backend, written in Rust.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// The source file to compile.
    input_file: String,

    /// The path for the output file.
    /// If not provided, it will be derived from the input file name (e.g., test.c -> test).
    #[arg(short, long)]
    output_file: Option<String>,

    /// Emit LLVM IR only instead of compiling to an executable.
    #[arg(long, short = 'S')]
    emit_llvm: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let input_path = Path::new(&cli.input_file);
    let source_name = input_path.to_str().unwrap_or("unknown_file");

    let output_path = match cli.output_file {
        Some(path) => path,
        None => input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| "a.out".to_string()),
    };

    let source_code = fs::read_to_string(input_path)
        .map_err(|e| format!("Failed to read file '{}': {}", cli.input_file, e))?;

    // --- Unified Compilation Pipeline ---

    // 1. Create the DiagnosticBag, which will collect errors from all stages.
    let mut diagnostics = DiagnosticBag::new(&source_code);

    // 2. Syntactic Analysis (includes lexical analysis).
    // parser::parse handles lexing and parsing errors, reporting them to diagnostics.
    let ast = match parser::parse(&source_code, &mut diagnostics) {
        Some(ast) => ast,
        None => {
            eprintln!("Compilation failed during parsing.");
            diagnostics.print(source_name);
            process::exit(1);
        }
    };

    // 3. Semantic Analysis
    // [FIX #1] The DiagnosticBag is now passed during the Analyzer's creation.
    let mut analyzer = Analyzer::new(&mut diagnostics);
    // [FIX #2] The analyze method no longer needs the diagnostics argument.
    let hir = match analyzer.analyze(&ast) {
        Some(hir) => hir,
        None => {
            eprintln!("Compilation failed during semantic analysis.");
            diagnostics.print(source_name);
            process::exit(1);
        }
    };

    // 4. Code Generation
    let llvm_ir = match codegen::codegen(&hir, &mut diagnostics) {
        Some(ir) => ir,
        None => {
            eprintln!("Compilation failed during code generation.");
            diagnostics.print(source_name);
            process::exit(1);
        }
    };

    // --- If compilation is successful, proceed with the next steps ---

    if cli.emit_llvm {
        fs::write(&output_path, llvm_ir)?;
        println!("Successfully generated LLVM IR at '{}'", output_path);
    } else {
        let temp_ll_file = format!("{}.ll", output_path);
        fs::write(&temp_ll_file, llvm_ir)?;

        println!("Invoking clang to compile and link...");
        let clang_status = Command::new("clang")
            .arg(&temp_ll_file)
            .arg("-o")
            .arg(&output_path)
            .status()?;

        if !clang_status.success() {
            return Err(format!("Clang failed with status: {}", clang_status).into());
        }

        fs::remove_file(&temp_ll_file)?;
        println!("Successfully compiled executable at '{}'", output_path);
    }

    Ok(())
}