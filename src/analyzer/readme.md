# CScript 语义分析器 (`analyzer`) 开发者文档

## 1. 模块概览 (Overview)

`analyzer` (语义分析器) 是 CScript 编译器的心脏和大脑。它扮演着承上启下的关键角色，是连接原始语法（AST）和可执行代码生成（Codegen）的核心桥梁。

- **输入 (Input):** `parser` 模块生成的**抽象语法树 (AST - `ast::Program`)**。AST 忠实地反映了源代码的语法结构，但没有经过类型检查或语义验证。

- **输出 (Output):** **高级中间表示 (HIR - `hir::Program`)**。HIR 是一个经过完全“注解”的、富含语义信息的树状结构。相比于 AST，它包含了每个表达式的**解析后类型**、每个变量的**存储位置**（全局或局部栈偏移）、以及对符号的直接引用（`Arc<VarDecl>`），为代码生成阶段提供了所有必要的信息。

- **核心任务 (Core Tasks):**
  - **符号解析 (Symbol Resolution):** 查找并验证变量、函数、类型等符号的定义。
  - **类型检查 (Type Checking):** 验证操作数和函数参数的类型是否匹配，例如，不能将浮点数和整数直接相加。
  - **语义验证 (Semantic Validation):** 执行一系列上下文相关的规则检查，例如：
    - `break` 或 `continue` 必须在循环内部。
    - 不能对常量进行赋值。
    - 结构体成员访问必须作用于结构体类型上。
  - **AST 到 HIR 的降级 (Lowering):** 将 AST 节点转换为信息更丰富的 HIR 节点。

## 2. 核心设计与架构 (Core Design & Architecture)

为了清晰地处理复杂的分析任务，我们采用了多趟扫描（Multi-pass）和基于 Trait 的访问者模式。

### 两遍扫描法 (Two-Pass Analysis)

CScript 语言支持函数和类型的前向引用（即在使用时定义还未出现），为了正确处理这种情况，分析器采用了两遍扫描法：

- **第一遍 (Pass 1): 符号收集**
  - **执行者:** `globals::pass1_collect_symbols`
  - **职责:** 快速遍历整个 AST 的顶层项目（全局变量、结构体定义、函数签名、`extern` 块），并将这些**全局符号**的名称和基本信息（如函数签名、结构体字段）注册到符号表的全局作用域中。此阶段不深入分析函数体或初始化表达式。
  - **目的:** 确保在进入第二遍的深度分析之前，所有全局名称都已可知。

- **第二遍 (Pass 2): 完整分析与降级**
  - **执行者:** `analyze` 方法中的后续调用流程
  - **职责:** 再次遍历 AST，这次进行全面的、递归的分析。它会利用第一遍收集到的符号信息，深入函数体和表达式内部，执行详细的类型检查和语义验证，并最终构建出完整的 HIR 树。

### 核心抽象 (Core Abstractions)

- **`Analyzer` (结构体 `mod.rs`)**
  - 作为状态的**所有者**，它拥有 `SymbolTable` 以及在整个分析过程中需要维护的状态（如 `loop_depth`、`current_function_return_type`）。
  - 其 `analyze()` 方法是顶层协调者，负责按顺序驱动上述的两遍扫描。

- **`AnalysisContext` (`AnaCtx`) (结构体 `mod.rs`)**
  - 一个临时的**上下文**结构体，包含了对 `Analyzer` 状态的**可变借用**。它在递归的 `lower` 调用链中被传递，为每个分析步骤提供了所需的环境信息（如当前作用域的符号表、诊断信息包、上下文期望的类型等），同时通过 Rust 的借用检查器保证了状态访问的绝对安全。

- **`Lower` (Trait `mod.rs`)**
  - 我们架构的核心。它定义了将 `ast` 节点降级为 `hir` 节点的统一行为。
  - **`lower<'a>(&self, ctx: &mut AnalysisContext<'a>) -> Option<Self::Output>`**: 通过让 `ast::Expression` 和 `ast::Statement` 实现这个 trait，我们将庞大的 `match` 逻辑分解到了各个独立的、可维护的辅助函数中。此处的 `for<'a>` (HRTB) 设计是整个架构能够优雅处理临时上下文和嵌套生命周期的关键。

## 3. 模块文件结构与职责

### `mod.rs` - 协调器
- **职责:** 模块的根文件，负责定义核心抽象 (`Analyzer`, `AnaCtx`, `Lower`)，声明所有子模块，并协调顶层的分析流程。**它不包含任何具体的表达式或语句分析逻辑。**

### `globals.rs` & `functions.rs` - 顶层项目处理器
- **`globals.rs`**:
  - **`pass1_collect_symbols(...)`**: 实现第一遍扫描的完整逻辑。
  - **`pass2_lower_global_initializers(...)`**: 在第二遍扫描中，专门负责处理全局变量的初始化表达式。
- **`functions.rs`**:
  - **`pass2_lower_functions(...)`**: 在第二遍扫描中，负责驱动所有函数定义的分析。
  - **`lower_function_definition(...)`**: 处理单个 `ast::FunctionDef` 的完整降级过程，包括创建新的符号作用域、处理参数、并调用 `statement.rs` 中的逻辑来分析函数体。

### `expression.rs` & `statement.rs` - AST 遍历核心
这两个模块是 `Lower` trait 的主要实现者，是第二遍扫描的工作核心。

- **`expression.rs`**:
  - **`impl Lower for ast::Expression`**: 作为分发器，将不同类型的 `ast::ExprKind` 委托给具体的 `lower_*` 辅助函数。
  - **辅助函数 (`lower_literal`, `lower_binary_op`, etc.)**: 每个函数负责一种表达式的完整分析，包括：
    - **递归调用**: 对子表达式调用 `.lower()`。
    - **类型检查**: 比较子表达式的类型，验证操作的合法性。
    - **HIR 构建**: 在所有检查通过后，创建对应的 `hir::Expression` 节点，并为其`resolved_type` 字段赋上正确的 `SemanticType`。

- **`statement.rs`**:
  - **`impl Lower for ast::Statement`**: 作为分发器，委托给具体的 `lower_*` 辅助函数。
  - **辅助函数 (`lower_var_decl`, `lower_if_statement`, etc.)**:
    - **作用域管理**: `lower_block` 会调用 `symbol_table.enter_scope()` 和 `exit_scope()`。
    - **状态管理**: `lower_while_statement` 会修改上下文中的 `loop_depth`。
    - **控制流分析**: `lower_if_statement` 等会检查其条件表达式的类型是否为 `bool`。

### `hir.rs`, `types.rs`, `symbols.rs`, `semantic_error.rs` - 基础支撑模块
这些是早已设计好的、职责单一的基础模块。

- **`hir.rs`**: **(输出)** 定义了 `hir::Program` 及其所有子结构。这是整个 `analyzer` 模块的**构建目标**。
- **`types.rs`**: 定义了 `SemanticType` 枚举和相关的辅助函数（如 `size_of`, `resolve_ast_type`）。它是类型检查的**核心依据**。
- **`symbols.rs`**: 定义了 `SymbolInfo` 和 `SymbolTable`。它是符号解析的**核心工具**。
- **`semantic_error.rs`**: 定义了所有可能发生的语义错误，并提供了到 `Diagnostic` 的转换。它是我们**高质量错误报告的基础**。

## 4. 工作流程示例 (Example Workflow)

让我们通过一个简单的语句 `let x: i32 = y + 10;` 来追踪其完整的分析流程：

1.  **Pass 1 (`globals.rs`)**: 假设这是一个全局声明，`pass1_collect_symbols` 会在全局作用域中注册一个名为 `x` 的 `SymbolInfo::Variable`，其类型为 `i32`，但初始化器为空。

2.  **Pass 2 (`globals.rs`)**: `pass2_lower_global_initializers` 遇到这个声明，发现它有一个初始化器 `y + 10`。
    a. 它创建一个 `AnalysisContext`，并将 `expected_type` 设置为 `Some(&SemanticType::Integer { width: 32, is_signed: true })`。
    b. 它调用 `(y + 10).lower(&mut ctx)`。

3.  **`expression.rs`**: `impl Lower for ast::Expression` 接收到调用，匹配到 `BinaryOp` 分支，并调用 `lower_binary_op(op, left, right, span, ctx)`。
    a. `lower_binary_op` 首先调用 `y.lower(ctx)`（此时 `expected_type` 为 `None`）。
    b. `impl Lower` 再次被调用，这次匹配到 `Variable` 分支，调用 `lower_variable(ident, span, ctx)`。
    c. `lower_variable` 在 `ctx.symbol_table` 中查找 `y`。假设找到了，它返回一个 `hir::Expression`，其中 `resolved_type` 是 `y` 的类型（比如 `i32`）。
    d. `lower_binary_op` 现在拿到了 `y` 的 HIR 和类型 `i32`。它接着为 `10` 的分析创建一个**新的临时上下文**，其中 `expected_type` 被设置为 `Some(&i32)`。
    e. 它调用 `10.lower(&mut temp_ctx)`。
    f. `impl Lower` 匹配到 `Literal` 分支，调用 `lower_literal`。`lower_literal` 看到期望类型是 `i32`，于是进行溢出检查，然后返回一个类型为 `i32` 的 `hir::Expression`。
    g. `lower_binary_op` 检查 `y` 和 `10` 的类型是否兼容（都是 `i32`），然后构建并返回一个代表 `y + 10` 的、`resolved_type` 也是 `i32` 的 `hir::Expression` 节点。

4.  **`globals.rs` (返回)**: `pass2_...` 收到 `y + 10` 的 HIR，将其存入 `hir::VarDecl` 的 `initializer` 字段，最终完成对此全局变量的完整分析。

这个流程清晰地展示了不同模块和函数如何通过 `Lower` trait 和 `AnalysisContext` 协同工作，完成复杂的分析任务。