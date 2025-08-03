# CScript 代码生成器 (`codegen`) 开发者文档

欢迎来到 CScript 编译器的代码生成模块！本文档旨在为开发者详细解释本模块的内部架构、设计哲学和工作流程。

## 概览 (Overview)

`codegen` 模块的核心职责是将语义分析阶段产出的**高级中间表示 (HIR)**，转换（或称“降级”）为 **LLVM IR**。我们采用了一种高度模块化、基于 Trait 的架构，以确保代码的清晰性、可维护性和高扩展性。

## 核心设计哲学 (Core Philosophy)

1.  **关注点分离 (Separation of Concerns):** 每个文件和模块都有单一、明确的职责。例如，`expression.rs` 只负责表达式的生成，`statement.rs` 只负责语句的生成。
2.  **基于 Trait 的分发 (Trait-Based Dispatch):** 我们不使用巨大、笨重的 `match` 语句来处理所有节点类型，而是定义了一个核心的 `GenerateHIR` trait。每个 HIR 节点类型通过实现这个 trait，来“宣告”自己应该如何被转换成 LLVM IR。这使得系统易于扩展（符合开闭原则）。
3.  **状态所有权清晰 (Clear State Ownership):**
      * `CodeGen` 结构体是所有 LLVM 对象和状态的**唯一所有者**。
      * `CodeGenCtx` 结构体是一个临时的**可变借用**，它将状态安全地传递给各个生成函数，避免了复杂的生命周期问题和对全局状态的依赖。

## 模块文件结构与职责

```
src/codegen/
├── README.md           # (本文档)
├── mod.rs              # 核心结构与协调器
├── expression.rs       # 表达式生成逻辑
├── statement.rs        # 语句生成逻辑
├── lvalue.rs           # 左值 (L-value) 生成逻辑
├── globals.rs          # 全局变量生成逻辑
├── function.rs         # 函数生成逻辑
└── utils.rs            # 可复用的辅助函数
```

-----

### `mod.rs` - 核心与协调器

这是整个模块的入口和大脑。它定义了我们的核心架构组件：

  * **`CodeGen` (结构体):**

      * **职责:** 作为状态的“所有者”，它拥有 `inkwell` 的核心对象（`Context`, `Module`, `Builder`）以及所有全局状态（`functions`, `global_variables` 等 `HashMap`）。
      * **`run()` 方法:** 这是代码生成的顶层入口。它不包含任何具体的生成逻辑，只负责按顺序协调调用其他模块的顶层函数，执行经典的“三遍扫描”流程：
        1.  `globals::generate_globals()`: 生成全局变量。
        2.  `function::generate_function_declarations()`: 生成所有函数（包括 extern）的声明。
        3.  `function::generate_function_bodies()`: 生成所有函数的函数体。

  * **`CodeGenCtx` (结构体):**

      * **职责:** 代码生成上下文。它是一个临时的、包含了对 `CodeGen` 状态的可变借用的结构体。它作为参数被传递给所有具体的生成函数，为它们提供所需的一切“工具”和“状态”，如 `builder`、`variables` 映射表等。

  * **`GenerateHIR` (Trait):**

      * **职责:** 定义了将 HIR 节点转换为 LLVM IR 的核心行为。所有可生成的 HIR 节点（如 `hir::Expression`, `hir::Statement`）都必须实现这个 trait。
      * **`generate()` 方法:** 这是该 trait 的核心方法，每个 HIR 节点通过实现它来定义自己的代码生成逻辑。

-----

### `expression.rs` - 表达式生成器

  * **职责:** 负责将所有 `hir::ExprKind` 降级为 LLVM IR 中的值 (`BasicValueEnum`)。
  * **核心实现:** `impl GenerateHIR for hir::Expression`。这个实现块扮演了一个**分发器**的角色，它通过 `match` 语句，将具体的生成任务委托给一系列专门的辅助函数。
  * **辅助函数:**
      * `generate_literal()`: 处理字面量（整数、浮点数、字符串等）。
      * `generate_binary_op()`: 处理二元运算（`+`, `-`, `==` 等）。
      * `generate_unary_op()`: 处理一元运算（`-`, `!` 等）。
      * `generate_variable()`: 处理变量加载，它会调用 `lvalue.rs` 中的逻辑来获取变量地址，然后加载其值。
      * `generate_function_call()`: 处理函数调用。
      * ...等等。

-----

### `statement.rs` - 语句生成器

  * **职责:** 负责处理所有 `hir::Statement`，将它们转换成带有控制流的 LLVM IR 指令。语句通常没有“值”。
  * **核心实现:** `impl GenerateHIR for hir::Statement`。与 `expression.rs` 类似，它也是一个分发器。
  * **辅助函数:**
      * `generate_var_decl()`: 处理局部变量声明，包括调用 `alloca` 在栈上分配内存。
      * `generate_if_statement()`: 处理 `if/else` 语句，负责创建 `then`, `else`, `merge` 等基本块并生成条件跳转指令。
      * `generate_while_statement()`: 处理 `while` 循环，负责创建 `cond`, `body`, `after` 等基本块，并管理循环栈。
      * `generate_return_statement()`: 处理 `return` 语句。
      * ...等等。

-----

### `lvalue.rs` - 左值 (L-value) 生成器

  * **职责:** 这是一个职责非常专一的模块，只负责计算表达式的**内存地址 (PointerValue)**，即所谓的“左值”。这对于变量赋值、取地址操作等至关重要。
  * **核心实现:** `impl GenerateLValue for hir::Expression`。我们为此定义了一个专门的 `GenerateLValue` trait。
  * **处理逻辑:**
      * 对于变量，它在符号表中查找其 `alloca` 指针。
      * 对于解引用表达式 `*ptr`，它计算 `ptr` 的值作为地址。
      * 对于成员访问 `s.field`，它先计算 `s` 的地址，然后使用 `GEP` (Get Element Pointer) 指令计算字段的地址。

-----

### `globals.rs` & `function.rs`

这两个模块负责处理顶层物品的生成，逻辑相对独立。

  * **`globals.rs`:**
      * `generate_globals()`: 遍历 HIR 中的所有全局变量，为它们创建 `GlobalValue`，并处理其（必须是常量的）初始化表达式。
  * **`function.rs`:**
      * `generate_function_declarations()`: 遍历所有函数（包括 `extern`），在 LLVM 模块中预先创建它们的声明（`FunctionValue`），以便函数之间可以相互调用。
      * `generate_function_bodies()`: 在所有函数都声明完毕后，再遍历一次，为每个函数生成其内部的完整代码。它会创建入口基本块，处理函数参数，然后调用 `statement.rs` 中的逻辑来填充函数体。

-----

### `utils.rs` - 工具箱

  * **职责:** 存放所有无状态的、可被整个 `codegen` 模块复用的辅助函数。
  * **示例函数:**
      * `to_llvm_type()`: 将 CScript 的 `SemanticType` 转换为对应的 `inkwell` 类型。
      * `create_entry_block_alloca()`: 一个重要的辅助函数，确保所有局部变量的 `alloca` 指令都被放在函数入口的第一个基本块中，这是 LLVM 官方推荐的最佳实践，非常有利于后续的优化。
      * `int_predicate()`, `float_predicate()`: 将我们的二元操作符转换为 LLVM 需要的比较谓词。

