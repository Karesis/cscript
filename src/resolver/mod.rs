// in src/resolver/mod.rs

use crate::parser::{ast::*, parse};
use crate::reporter::{CompilerError, ResolverError};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};

/// 一个表示整个 CScript 项目的数据结构。
#[derive(Debug, Default)]
pub struct Project {
    /// 将模块的规范化路径映射到其 AST。
    /// 使用 PathBuf 作为键，可以唯一标识每个文件。
    pub modules: HashMap<PathBuf, Program>,
    // 我们可以添加一个字段来存储入口模块的路径，以便后续阶段知道从哪里开始
    pub entry_module_path: Option<PathBuf>,
}

/// 模块解析器。
/// 负责从入口文件开始，发现并解析项目中的所有源文件。
pub struct ModuleResolver {
    /// 项目的根目录，用于解析 `root::` 路径。
    root_dir: PathBuf,
    /// 工作队列，存放待处理的模块路径。
    work_queue: VecDeque<PathBuf>,
    /// 已经处理过或已在队列中的模块，防止重复处理和循环依赖。
    processed_modules: HashSet<PathBuf>,
    /// 在解析过程中收集到的所有错误。
    errors: Vec<CompilerError>,
}

impl ModuleResolver {
    /// 创建一个新的 ModuleResolver 实例。
    ///
    /// # 参数
    /// * `entry_path` - 编译入口文件的路径。
    pub fn new(entry_path: &Path) -> Self {
        // 我们将入口文件所在的目录假定为项目的根目录。
        // 这是一个合理的默认行为，未来可以通过配置文件（如 Cscript.toml）使其更灵活。
        let root_dir = entry_path.parent().unwrap_or(Path::new("")).to_path_buf();
        Self {
            root_dir,
            work_queue: VecDeque::new(),
            processed_modules: HashSet::new(),
            errors: Vec::new(),
        }
    }

    /// 模块解析的主函数。
    pub fn resolve(mut self, entry_path: &Path) -> Result<Project, Vec<CompilerError>> {
        let mut project = Project::default();

        // --- 1. 初始化 ---
        // 将入口文件路径规范化（转换为绝对路径或一个清晰的相对路径）。
        // 这对于作为 HashMap 的键至关重要，能确保路径的唯一性。
        let canonical_entry_path = fs::canonicalize(entry_path).expect("Entry file path is invalid");
        project.entry_module_path = Some(canonical_entry_path.clone());

        // 将入口文件加入工作队列和已处理集合。
        self.work_queue.push_back(canonical_entry_path.clone());
        self.processed_modules.insert(canonical_entry_path);

        // --- 2. 主循环 ---
        // 只要工作队列不为空，就继续处理。
        while let Some(current_path) = self.work_queue.pop_front() {
            // -- A. 读取文件 --
            let source = match fs::read_to_string(&current_path) {
                Ok(s) => s,
                Err(e) => {
                    // 如果文件读取失败，记录错误并跳过这个文件。
                    self.errors.push(
                        ResolverError::FileReadError {
                            path: current_path.to_string_lossy().to_string(),
                            io_error: e.to_string(),
                            // 注意：此时我们没有一个好的 `use` 语句的 span，这是一个待改进点。
                            // 暂时使用一个零宽度的 span。
                            span: (0, 0).into(),
                        }
                        .into(),
                    );
                    continue; // 继续处理队列中的下一个文件
                }
            };
            
            // -- B. 解析文件 --
            // 1. 词法分析
            let (tokens, lex_errors) = crate::lexer::lex(&source);
            self.errors.extend(lex_errors);

            // 2. 语法分析
            let (ast_opt, parse_errors) = crate::parser::parse(source.len(), tokens);
            self.errors.extend(parse_errors);

            let ast = match ast_opt {
                Some(program) => program,
                None => {
                    // 如果解析失败（返回 None），我们已经记录了错误，直接跳过。
                    continue;
                }
            };

            // -- C. 发现并处理依赖 --
            for item in &ast.items {
                if let GlobalItem::Use(use_decl) = item {
                    match self.resolve_use_path(use_decl, &current_path) {
                        Ok(Some(new_path)) => {
                            // 如果成功解析出一个新路径，并且我们从未处理过它...
                            if self.processed_modules.insert(new_path.clone()) {
                                // ...就把它加入工作队列。
                                self.work_queue.push_back(new_path);
                            }
                        }
                        Err(e) => self.errors.push(e),
                        _ => {} // Ok(None) 表示路径有效但无需处理 (例如，外部库)
                    }
                }
            }

            // -- D. 存储 AST --
            project.modules.insert(current_path, ast);
        }

        // --- 3. 返回结果 ---
        if self.errors.is_empty() {
            Ok(project)
        } else {
            Err(self.errors)
        }
    }

    /// 辅助函数，根据 `use` 声明和当前文件路径，解析出依赖模块的规范化路径。
    ///
    /// 这是模块解析的核心逻辑所在。
    fn resolve_use_path(
        &self,
        use_decl: &UseDecl,
        current_file_path: &Path,
    ) -> Result<Option<PathBuf>, CompilerError> {
        // --- 1. 确定基准路径 ---
        let base_path = match use_decl.path.kind {
            PathKind::Root => self.root_dir.clone(),
            PathKind::Super => {
                // `current_file_path` -> parent (dir) -> parent (super dir)
                if let Some(dir) = current_file_path.parent() {
                    dir.parent().unwrap_or(Path::new("")).to_path_buf()
                } else {
                    PathBuf::new() // Should not happen if path is valid
                }
            }
            PathKind::Relative => {
                // The directory containing the current file
                if let Some(dir) = current_file_path.parent() {
                    dir.to_path_buf()
                } else {
                    PathBuf::new()
                }
            }
        };

        // --- 2. 将路径分段组合成相对路径 ---
        let mut relative_module_path = PathBuf::new();
        for segment in &use_decl.path.segments {
            relative_module_path.push(&segment.name);
        }

        // --- 3. 组合基准路径和相对路径，得到候选模块的基本路径 ---
        let candidate_base = base_path.join(relative_module_path);

        // --- 4 & 5. 尝试两种可能的解析方式 ---
        let mut resolved_path: Option<PathBuf> = None;

        // 尝试 1: `.../module.cx`
        let file_candidate = candidate_base.with_extension("cx");
        if file_candidate.is_file() {
            resolved_path = Some(file_candidate);
        } else {
            // 尝试 2: `.../module/init.cx`
            let dir_candidate = candidate_base.join("init.cx");
            if dir_candidate.is_file() {
                resolved_path = Some(dir_candidate);
            }
        }
        
        // --- 6 & 7. 处理解析结果 ---
        match resolved_path {
            Some(found_path) => {
                // 成功找到文件，现在将其规范化
                match fs::canonicalize(&found_path) {
                    Ok(canonical_path) => Ok(Some(canonical_path)),
                    Err(e) => {
                        // 规范化失败，这是一个文件系统级别的错误
                        Err(ResolverError::CanonicalizationError {
                            path: found_path.to_string_lossy().to_string(),
                            io_error: e.to_string(),
                            span: use_decl.span.clone().into(),
                        }
                        .into())
                    }
                }
            }
            None => {
                // 两种方式都失败了，报告 ModuleNotFound 错误
                Err(ResolverError::ModuleNotFound {
                    path: format_use_path(&use_decl.path),
                    span: use_decl.span.clone().into(),
                }
                .into())
            }
        }
    }
}

/// 辅助函数，将 UsePath AST 节点格式化为可读的字符串，用于错误报告。
fn format_use_path(path: &UsePath) -> String {
    let mut s = String::new();
    match path.kind {
        PathKind::Root => s.push_str("root::"),
        PathKind::Super => s.push_str("super::"),
        _ => {}
    }
    s.push_str(
        &path.segments
            .iter()
            .map(|ident| ident.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    );
    s
}