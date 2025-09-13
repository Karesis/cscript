# CScript

**A modern systems language with a C soul and a Rust mind, built for a new era of development.**

CScript is a new, from-scratch compiler for a systems programming language that combines the low-level control of C with the modern syntax and safety features inspired by Rust. It's designed for developers who love C's simplicity and performance but crave a more expressive, less error-prone development experience.

This project is built in Rust, leveraging a world-class toolchain to achieve an industrial-grade standard:

*   **Lexical Analysis**: `logos`
*   **Syntactic Analysis**: `chumsky`
*   **Semantic Analysis**: Hand-written, multi-pass analysis
*   **Code Generation**: `inkwell` (LLVM)
*   **Error Reporting**: `miette` + `thiserror` (for beautiful, ergonomic, and structured diagnostics)

---

## ✨ Why CScript?

CScript isn't just another C dialect. It's a reimagining of what C could be, guided by a few core principles:

### 💖 Developer-First Diagnostics

Clear, precise error messages are not an afterthought—they are a core feature. We believe a great language needs a great developer experience, and that starts with world-class diagnostics.

After a major architectural refactor, our entire error reporting pipeline is now powered by **`miette`** and **`thiserror`**. This isn't just a cosmetic change; it's a fundamental upgrade:

*   **Structured & Centralized**: All compiler errors, from every stage, are now defined as structured data. This eliminates inconsistent, ad-hoc error messages.
*   **Context-Aware**: By tracking precise source spans (`Span`) throughout the entire compilation process, `miette` can render beautiful, colorful reports that highlight not just *where* an error is, but also provide contextual labels and helpful suggestions.
*   **Ergonomic**: `thiserror` makes defining new, complex errors trivial, ensuring that our compiler's diagnostic capabilities can easily grow with the language.

The result is a compiler that respects your time and helps you get back to coding faster.

### 🧠 Context-Driven Type Ascription

In CScript, a literal such as `10` or `3.14` is itself **"typeless."** It is merely a representation of a potential value and carries no specific type information until it is used.

The type is bestowed at the moment of "ascription." In the statement `x: u8 = 10;`, the `: u8` is not a simple "annotation" or a form of "type inference." Instead, it is the core action of **Type Ascription**. It takes the typeless integer literal `10` and **confirms and initializes** it as a concrete value of type `u8`.

This philosophy of **"Type Annotation is Initialization"** ensures that every value has a clear, unambiguous type, enables precise compile-time checks (like overflow detection), and fundamentally eliminates the need for verbose casting syntax.

### 🛠️ Modern, Unambiguous Syntax

We use a `name: type` declaration style inspired by modern languages. This eliminates classic C ambiguities (like the [most vexing parse](https://en.wikipedia.org/wiki/Most_vexing_parse)) and makes the code cleaner and easier to read.

---

## 📚 Features at a Glance

*   **Rich Type System**: A full range of integer (`i8`-`i64`), float (`f32`/`f64`), `bool`, and `char` types.
*   **Pointers**: First-class support for pointers, enabling low-level memory manipulation.
*   **Modern Structs**: Define complex data types with a clean, Rust-style syntax.
*   **Seamless C Interoperability (FFI)**: Call C standard library functions or your own C code directly via `extern "C"` blocks.
*   **Memory Safety (Compile-Time)**: Overflow checking for all numeric literals based on their context.
*   **Advanced Control Flow**: `if/else`, `while`, `return`, `break`, `continue`, and nested block scopes.
*   **LLVM Backend**: Compiles to high-performance, platform-native code via LLVM.

---

## 🚀 A Quick Tour

Let's see what CScript can do. Here's an example showcasing structs, functions, and modern syntax. Save this as `tour.cx`:

```c
// tour.cx
// A quick tour of CScript's modern syntax and struct support.

struct Point {
    x: i32,
    y: i32
}

// Pass struct by value
sum(p: Point) -> i32 {
    return p.x + p.y;
}

// Pass struct by pointer
move(p_ptr: Point*) -> void {
    (*p_ptr).x = (*p_ptr).x + 10;
}

main() -> i32 {
    // Instantiate with a C-style aggregate literal.
    // The type is determined by the context.
    my_point: Point = { 5, 10 };
    
    initial_sum: i32 = sum(my_point); // Should be 15
    
    move(&my_point); // my_point is now {15, 10}
    
    // The final result should be 15 + 15 = 30
    return my_point.x + initial_sum;
}
```

### Compile and Run

The CScript compiler can be run directly via Cargo. It will compile your source file and produce an LLVM IR file (`.ll`).

```bash
# Compile the program to LLVM IR
cargo run -- tour.cx

# This generates 'a.ll' by default. Now, use clang to create an executable:
clang a.ll -o tour_app

# Run the executable and check its exit code
./tour_app
echo $? # Should output 30
```

---

## 🔗 Interacting with C (FFI)

One of CScript's core strengths is its compatibility with the C ecosystem. Through the Foreign Function Interface (FFI), you can effortlessly call battle-tested C libraries.

Here's how you can call the `printf` function from the C standard library. Save this as `ffi_example.cx`:

```c
// ffi_example.cx

extern "C" {
    printf(format: char*, ...) -> i32;
}

main() -> i32 {
    num: i32 = 42;
    // Call a C function just like it's a native one
    printf("Hello from CScript! The magic number is %d\n", num);
    return 0;
}
```

### Compile and Run

```bash
# Compile to 'a.ll'
cargo run -- ffi_example.cx

# Link and run with clang
clang a.ll -o ffi_app
./ffi_app
# Expected output: Hello from CScript! The magic number is 42
```

---

## 🔧 Installation

### Build from Source

1.  Ensure you have the [Rust toolchain](https://rustup.rs/) and a C compiler like `clang` installed.
2.  Clone this repository:
    ```bash
    git clone https://github.com/your-username/cscript.git
    cd cscript
    ```
3.  Build the project:
    ```bash
    cargo build --release
    ```
    You can then run the compiler using `cargo run --release -- ...`.

---

## 🗺️ The Road Ahead

Our journey is accelerating! The recent architectural refactor has paved the way for more rapid development. The next steps are focused on expanding the language's power and ecosystem:

*   [ ] **Comprehensive Testing**: Add extensive unit and integration tests for all existing features, including structs and FFI.
*   [ ] **Compiler Driver & Build System**: Create a standalone executable (`cscriptc`) and a simple build system to handle multi-file projects.
*   [ ] **Arrays & Slices**: Implement first-class support for fixed-size arrays and potentially dynamic slices.
*   [ ] **Enums & Tuples**: Add more expressive data structures inspired by modern languages.
*   [ ] **Module System**: Design and implement a simple file-based module system.
*   [ ] **Optimization**: Integrate more of LLVM's powerful optimization passes.

---

## 🙌 Contributing

CScript is an ambitious open-source project, and we welcome contributions of all kinds! Whether it's reporting a bug, adding a test case, suggesting a feature, improving documentation, or writing code, your help is valued.

Please feel free to open an issue or submit a pull request.

---

## 📜 License

This project is licensed under the Apache License, Version 2.0. A copy of the license is available in the repository's `LICENSE` file and at: [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)
