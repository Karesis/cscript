# **CScript**

**一个拥有 C 语言之魂和 Rust 思想的现代系统级语言，为新时代的开发而生。**

CScript 是一个全新的、从零开始构建的系统级编程语言编译器。它融合了 C 语言的底层控制能力与源自 Rust 的现代语法及安全特性，专为那些钟爱 C 的简洁与性能，同时又渴望更具表达力、更少出错的开发体验的开发者设计。

该项目使用 Rust 构建，利用其世界级的工具链以达到工业级标准：

  * **词法分析 (Lexical Analysis)**: `logos`
  * **语法分析 (Syntactic Analysis)**: `chumsky`
  * **语义分析 (Semantic Analysis)**: 手写访问者模式 (Visitor Pattern)
  * **代码生成 (Code Generation)**: `inkwell` (LLVM)
  * **错误报告 (Error Reporting)**: `ariadne`

-----

## ✨ **为什么选择 CScript?**

CScript 不仅仅是 C 的又一个方言，它是对 C 语言潜力的一次重新构想，遵循以下几个核心原则：

### **🧠 上下文驱动的类型确认 (Context-Driven Type Ascription)**

在 CScript 中，我们严格区分“字面量”和“类型化值”。像 `10` 或 `3.14` 这样的字面量，其本身是 **“无类型”** 的，它们仅仅是潜在值的表示，在被使用前不携带具体的类型信息。

类型的赋予发生在“确认”阶段。在 `x: u8 = 10;` 这行代码中，`: u8` 并非简单的“标注”或进行“类型推断”，而是一个**类型确认 (Type Ascription)** 的核心动作。它将无类型的整数字面量 `10` **确认并初始化**为一个具体的 `u8` 类型的值。

这个过程确保了所有值都在其生命周期开始时就拥有一个明确、无歧义的类型。我们称之为 **“类型标注即初始化”**。这种设计不仅在编译期实现了对字面量（如 `10` 是否溢出 `u8`）的精确检查，也从根本上消除了对繁琐类型转换语法的需求。

### **💖 开发者优先的诊断信息 (Developer-First Diagnostics)**

清晰、精确的错误信息不再是事后弥补。借助 `ariadne`，我们的诊断系统能够精确指向问题所在，让调试过程更快速、更直观。

### **🛠️ 现代、无歧义的语法 (Modern, Unambiguous Syntax)**

我们采用了源自现代语言的 `name: type` 声明风格。这消除了 C 语言的经典歧义（例如[最令人烦恼的解析](https://en.wikipedia.org/wiki/Most_vexing_parse)），使代码更整洁、更易于阅读。

-----

## 📚 **功能一览**

  * **丰富的类型系统**: `i8` 至 `i64`, `u8` 至 `u64`, `f32`/`f64`, `bool`, `char` 以及指针。
  * **现代结构体**: 使用简洁的、类 Rust 的语法定义复杂数据类型，并用 C 风格的聚合字面量进行实例化。
  * **无缝的 C 语言互操作性 (FFI)**: 通过 `extern "C"` 块，可以直接调用 C 标准库或你自己的 C 函数。
  * **内存安全 (编译时)**: 基于上下文对所有数字字面量进行溢出检查。
  * **高级控制流**: `if/else`, `while`, `return`, `break`, `continue` 以及嵌套的代码块作用域。
  * **LLVM 后端**: 即时编译 (JIT) 生成高性能的本地可执行文件。
  * **完整的工具链**: 从源代码到可执行文件，整个编译流程健壮且经过测试。

-----

## 🚀 **快速上手**

让我们看看 CScript 的能力。下面的例子展示了结构体、函数和现代语法。请将代码保存为 `tour.cx`：

```rust
// tour.cx
// CScript 现代语法与结构体支持的快速概览

struct Point {
    x: i32,
    y: i32
}

// 通过值传递结构体
sum(p: Point) -> i32 {
    return p.x + p.y;
}

// 通过指针传递结构体
move(p_ptr: Point*) -> void {
    (*p_ptr).x = (*p_ptr).x + 10;
}

main() -> i32 {
    // 使用聚合字面量进行实例化
    // 类型由上下文决定
    my_point: Point = { 5, 10 };
    
    initial_sum: i32 = sum(my_point); // 应该是 15
    
    move(&my_point); // my_point 现在是 {15, 10}
    
    // 最终结果应该是 15 + 15 = 30
    return my_point.x + initial_sum;
}
```

### **编译并运行**

```bash
# 编译程序
cargo run -- tour.cx -o tour_app

# 运行可执行文件
./tour_app
echo $? # 应该输出 30
```

-----

## 🔗 **与 C 语言交互 (FFI)**

CScript 的核心优势之一是其与 C 生态系统的兼容性。通过外部函数接口 (FFI)，你可以毫不费力地调用久经考验的 C 库，将 CScript 的现代特性与 C 的庞大生态系统结合起来。

下面的例子展示了如何调用 C 标准库中的 `printf` 函数。请将代码保存为 `ffi_example.cx`：

```rust
// ffi_example.cx

extern "C" {
    printf(format: char*, ...) -> i32;
}

main() -> i32 {
    num: i32 = 42;
    // 直接调用 C 函数，就像它是原生函数一样
    printf("Hello from CScript! The magic number is %d\n", num);
    return 0;
}
```

### **编译并运行**

```bash
# 编译
cargo run -- ffi_example.cx -o ffi_app

# 运行
./ffi_app
# 屏幕会输出: Hello from CScript! The magic number is 42
```

-----

## 🔧 **安装**

### **从源码构建**

1.  确保你已经安装了 [Rust 工具链](https://rustup.rs/) 和 `clang`。
2.  克隆本仓库：
    ```bash
    git clone https://github.com/karesis/cscript.git
    cd cscript
    ```
3.  构建 release 版本可执行文件：
    ```bash
    cargo build --release
    ```
    可执行文件将位于 `target/release/cscript`。

-----

## 🗺️ **未来规划**

我们的旅程正在加速！后续步骤将聚焦于扩展语言的能力和生态系统。

  * [ ] **数组与切片**: 实现对定长数组和（可能）动态切片的一等公民支持。
  * [ ] **枚举与元组**: 添加更多源自现代语言的、富有表达力的数据结构。
  * [ ] **模块系统**: 设计并实现一个简单的、基于文件的模块系统。
  * [ ] **优化**: 集成更多 LLVM 强大的优化遍 (passes)。

-----

## 🙌 **如何贡献**

CScript 是一个宏大的开源项目，我们欢迎各种形式的贡献！无论是报告 Bug、建议新功能、改进文档还是编写代码，你的帮助都至关重要。

请随时创建 Issue 或提交 Pull Request。

-----

## 📜 **许可证**

本项目采用Apache-2.0 License:

  * Apache License, Version 2.0 ([LICENSE-APACHE](https://www.google.com/search?q=LICENSE-APACHE) or [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))