CScript: 一个用 Rust 构建的现代编译器
项目愿景

CScript 起源于一个 C 语言编译器，但它的目标是超越 C。我借鉴 Rust 等现代语言的设计哲学，致力于将 CScript 发展为一门语法现代、类型安全、对开发者友好的系统级编程语言。

本项目从零开始，使用 Rust 语言和世界一流的编译器工具链构建，目标是达到工业级标准：

    词法分析: logos - 极速的词法分析器生成器。

    语法分析: chumsky - 表现力强大的解析器组合子库。

    语义分析: 手动实现的访问者模式，拥有上下文驱动的类型确定能力。

    代码生成: inkwell - LLVM 的安全 Rust 封装。

    错误报告: ariadne - 生成美观、精确的编译诊断信息。

项目目前已经完成了从源代码到原生可执行文件的完整编译流程，并拥有一个健壮的、贯穿始终的错误报告系统。
编译器当前状态与语言特性

CScript 已经从一个简单的 C 语言子集，演变为拥有自己独特语法的独立语言。核心功能均已实现并通过端到端测试。
1. 现代语法 (Modern Syntax)

CScript 采用了更清晰、更不易出错的类型后置语法：

    变量声明: variable_name: type = value;

    x: i32 = 10;
    PI: f64 = 3.14;

    函数定义: function_name(param: type) -> return_type { ... }

    add(a: i32, b: i32) -> i32 {
        return a + b;
    }

2. 丰富的原生类型系统 (Rich Primitive Types)

    有符号整数: i8, i16, i32 (或 int), i64

    无符号整数: u8, u16, u32, u64

    浮点数: f32, f64

    其他类型: char, bool, void

    指针: 支持任意层级的指针，例如 i32*, char**。

3. 语句 (Statements)

    变量声明: 支持局部和全局变量的声明与初始化。

    const 限定符。

    控制流:

        if / else 条件语句。

        while 循环语句。

        块语句: 支持任意嵌套的代码块 { ... } 作为独立语句。

    跳转语句: return, break, continue。

    表达式语句: 任何合法的表达式后加分号。

4. 表达式 (Expressions)

    字面量:

        整数字面量: 根据上下文确定类型 (e.g., 在 x: u8 = 10; 中, 10 被推断为 u8)。

        浮点数字面量: (e.g., 3.14159)。

        布尔字面量 (true, false)。

        字符串字面量 ("hello", 类型为 char*)。

    运算符:

        算术: +, -, *, /, % (支持整数和浮点数)。

        比较: ==, !=, >, <, >=, <= (支持整数和浮点数)。

        逻辑: &&, || (注意：当前为非短路求值)。

        一元: - (取反), ! (逻辑非), & (取地址), * (解引用)。

        赋值: =。

    函数调用: 支持带参数的函数调用。

5. 其他核心特性

    作用域: 正确处理全局、函数及块级作用域。

    类型安全: 在编译期进行严格的类型检查，包括对数字字面量的溢出检查。

    注释: 支持单行注释 // 和多行注释 /* ... */。

暂不支持的主要特性

    struct, union, enum

    数组的声明与访问

    for 循环 (可以被 while 替代)

    typedef 和类型别名

    显式类型转换 (Casts)

    预处理器

安装
从源码构建

    确保您已安装 Rust 工具链 和 clang。

    克隆本仓库:

    git clone https://github.com/karesis/cscript.git
    cd cscript

    构建 Release 版本的可执行文件:

    cargo build --release

    可执行文件位于 target/release/cscript。

使用方法

假设您有一个使用 CScript 新语法的 fib.c 文件：

// fib.c
// 使用 CScript 的现代语法编写的斐波那契函数

fib(n: i32) -> i32 {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

main() -> i32 {
    return fib(10); // 结果应该是 55
}

编译成可执行文件

# 默认输出名为 fib
cargo run -- fib.c

# 运行
./fib
echo $? # 应该输出 55

指定输出文件名

cargo run -- fib.c -o my_fib_program
./my_fib_program
echo $? # 应该输出 55

生成 LLVM IR

如果您想查看编译器生成的中间代码，可以使用 -S 或 --emit-llvm 标志。

cargo run -- fib.c -S -o fib.ll

这将会生成一个名为 fib.ll 的文本文件，其中包含了 fib.c 对应的 LLVM IR。
未来计划

我的征途是星辰大海！

    [ ] 高级数据结构: 实现对 struct 和数组的完整支持。

    [ ] 类型推断: 为变量声明引入 let 关键字，实现 let x = 10; 这样的类型推断。

    [ ] 模块系统: 设计并实现一个简单的文件模块系统。

    [ ] 优化与性能: 集成更多 LLVM 优化趟 (Optimization Passes)，并对编译器自身进行性能分析。

    [ ] 语言生态: 探索构建标准库、包管理器等可能性。

欢迎任何形式的贡献，让我们一起把 CScript 打造成一门令人兴奋的新语言！