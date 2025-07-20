# cscript: 一个用 Rust 构建的 C 语言编译器

[![构建状态](https://img.shields.io/badge/build-passing-brightgreen)](https://github.com/karesis/cscript)
[![许可证](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue)](https://opensource.org/licenses/MIT)

这是一个从零开始、以构建工业级编译器为目标的C语言编译器项目。它使用 Rust 语言编写，并整合了现代编译器开发的强大工具链：

* **词法分析**: `logos`
* **语法分析**: `chumsky`
* **语义分析**: 手动实现的访问者模式
* **代码生成**: `inkwell` (LLVM 的 Rust 安全封装)

项目目前已经完成了从源代码到原生可执行文件的完整编译流程。

## 编译器当前状态

编译器目前支持一个C语言的子集，类似于 `C0` 或教学中使用的核心语言。核心功能已经实现并通过了端到端测试。

## 当前支持的C语言范围

### 1. 数据类型 (Types)
* `int`: 32位有符号整数。
* `char`: 8位整数（字符）。
* `bool`: 布尔类型，值为 `true` 或 `false`。
* `void`: 用于函数返回类型。
* `*` (指针): 支持任意层级的指针，例如 `int*`, `char**`。

### 2. 语句 (Statements)
* **变量声明**: 支持局部和全局变量的声明与初始化 (`int x = 10;`)。
* `const` 限定符。
* **控制流**:
    * `if / else` 条件语句。
    * `while` 循环语句。
* **跳转语句**:
    * `return` (支持带/不带返回值)。
    * `break` (必须在循环内)。
    * `continue` (必须在循环内)。
* **表达式语句**: 任何合法的表达式后加分号（如赋值、函数调用）。

### 3. 表达式 (Expressions)
* **字面量**:
    * 整数字面量 (e.g., `123`)。
    * 布尔字面量 (`true`, `false`)。
    * 字符串字面量 (`"hello"`, 类型为 `char*`)。
* **变量读写**。
* **运算符**:
    * **算术运算符**: `+`, `-`, `*`, `/`, `%`。
    * **比较运算符**: `==`, `!=`, `>`, `<`, `>=`, `<=`。
    * **逻辑运算符**: `&&`, `||` (注意：当前为非短路求值)。
    * **一元运算符**: `-` (取反), `!` (逻辑非), `&` (取地址), `*` (解引用)。
    * **赋值运算符**: `=`。
* **函数调用**: 支持带参数的函数调用。

### 4. 其他特性
* **函数定义**: 支持带参数和返回值的函数。
* **作用域**: 正确处理全局作用域、函数作用域和块级作用域。
* **注释**: 支持单行注释 `//` 和多行注释 `/* ... */`。

### 暂不支持的主要特性
* `struct`, `union`, `enum`
* 数组的声明与访问
* `for` 循环 (可以被 `while` 替代)
* `typedef`
* 类型转换 (Casts)
* 预处理器

## 安装

### 从源码构建
1.  确保您已安装 [Rust 工具链](https://rustup.rs/) 和 `clang`。
2.  克隆本仓库:
    ```bash
    git clone [https://github.com/your-username/cscript.git](https://github.com/your-username/cscript.git)
    cd cscript
    ```
3.  构建 Release 版本的可执行文件:
    ```bash
    cargo build --release
    ```
    可执行文件位于 `target/release/cscript`。

## 使用方法

假设您有一个 `test.c` 文件：
```c
// test.c
int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    return fib(10); // 结果应该是 55
}
```

#### 编译成可执行文件
```bash
# 默认输出名为 test
cargo run -- test.c

# 运行
./test
echo $? # 应该输出 55
```

#### 指定输出文件名
```bash
cargo run -- test.c -o my_fib_program
./my_fib_program
echo $? # 应该输出 55
```

#### 生成 LLVM IR
如果您想查看编译器生成的中间代码，可以使用 `-S` 或 `--emit-llvm` 标志。
```bash
cargo run -- test.c -S -o test.ll
```
这将会生成一个名为 `test.ll` 的文本文件，其中包含了 `test.c` 对应的 LLVM IR。

## 未来计划
- [ ] 实现完整的短路求值逻辑运算符。
- [ ] 支持数组和 `struct`。
- [ ] 集成 LLVM 优化趟 (Optimization Passes)。
- [ ] 进一步完善 `ariadne` 的语义错误报告。