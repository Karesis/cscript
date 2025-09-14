// In src/main.rs

use clap::Parser;
use miette::{Report, NamedSource}; // <-- 导入 miette 的关键部分
use std::fs;
use std::path::PathBuf;

use cscript::{compile, reporter::CompilerError};

/// CScript 语言的编译器
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// 要编译的源文件
    #[arg()]
    input_file: PathBuf,

    /// 指定输出文件名
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// 将生成的 LLVM IR 打印到控制台，而不是写入文件
    #[arg(long, default_value_t = false)]
    emit_llvm: bool,
}

/// [NEW] 使用 miette 漂亮地打印所有编译错误。
fn report_errors(source_code: &str, errors: Vec<CompilerError>, file_name: &str) {
    eprintln!("Compilation failed with {} error(s):", errors.len());
    for error in errors {
        // 1. 将我们的 CompilerError 包装进 miette::Report
        // 2. 使用 .with_source_code() 附加源代码上下文
        let report = Report::new(error)
            .with_source_code(NamedSource::new(file_name, source_code.to_string()));
        
        // 3. 使用 {:?} 打印，miette 会自动进行漂亮的渲染！
        eprintln!("{:?}", report);
    }
}

fn main() {
    let args = Args::parse();
    let file_name = args.input_file.to_string_lossy();

    let source_code = match fs::read_to_string(&args.input_file) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading input file '{}': {}", file_name, e);
            std::process::exit(1);
        }
    };

    match compile(&source_code) {
        Ok(llvm_ir) => {
            if args.emit_llvm {
                println!("{}", llvm_ir);
            } else {
                let output_path = args.output.unwrap_or_else(|| PathBuf::from("a.ll"));
                if let Err(e) = fs::write(&output_path, llvm_ir) {
                    eprintln!("Error writing output file '{}': {}", output_path.display(), e);
                    std::process::exit(1);
                }
                println!("Compilation successful. Output written to {}.", output_path.display());
            }
        }
        Err(errors) => {
            report_errors(&source_code, errors, &file_name);
            std::process::exit(1);
        }
    }
}