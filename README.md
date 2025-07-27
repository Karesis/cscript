# **CScript 🚀**

**A modern systems language with a C soul and a Rust mind, built for a new era of development.**  
CScript is a new, from-scratch compiler for a systems programming language that combines the low-level control of C with the modern syntax and safety features inspired by Rust. It's designed for developers who love C's simplicity and performance but crave a more expressive, less error-prone development experience.  
This project is built in Rust, leveraging a world-class toolchain to achieve an industrial-grade standard:

* **Lexical Analysis**: logos  
* **Syntactic Analysis**: chumsky  
* **Semantic Analysis**: Hand-written Visitor Pattern  
* **Code Generation**: inkwell (LLVM)  
* **Error Reporting**: ariadne

## **✨ Why CScript?**

CScript isn't just another C dialect. It's a reimagining of what C could be, guided by a few core principles:

### **🧠 Context-Driven Typing**

Literals are flexible and powerful. In x: u8 \= 10;, the integer 10 is intelligently understood as a u8, with compile-time overflow checks. No more verbose casting for simple cases.

### **💖 Developer-First Diagnostics**

Clear, precise error messages are not an afterthought. Powered by ariadne, our diagnostic system points you exactly to the problem, making debugging faster and more intuitive.

### **🛠️ Modern, Unambiguous Syntax**

We use a name: type declaration style inspired by modern languages. This eliminates classic C ambiguities (like the [most vexing parse](https://en.wikipedia.org/wiki/Most_vexing_parse)) and makes the code cleaner and easier to read.

## **📚 Features at a Glance**

* **Rich Type System**: i8-i64, u8-u64, f32/f64, bool, char, and pointers.  
* **Modern Structs**: Define complex data types with a clean, Rust-style syntax and instantiate them with C-style aggregate literals.  
* **Memory Safety (Compile-Time)**: Overflow checking for all numeric literals based on their context.  
* **Advanced Control Flow**: if/else, while, return, break, continue, and nested block scopes.  
* **LLVM Backend**: JIT compilation to high-performance, native executables.  
* **Complete Toolchain**: From source code to executable, the entire pipeline is robust and tested.

## **🚀 A Quick Tour**

Let's see what CScript can do. Here's an example showcasing structs, functions, and modern syntax. Save this as tour.csc:  
// tour.csc  
// A quick tour of CScript's modern syntax and struct support.

struct Point {  
    x: i32,  
    y: i32  
}

// Pass struct by value  
sum(p: Point) \-\> i32 {  
    return p.x \+ p.y;  
}

// Pass struct by pointer  
move(p\_ptr: Point\*) \-\> void {  
    (\*p\_ptr).x \= (\*p\_ptr).x \+ 10;  
}

main() \-\> i32 {  
    // Instantiate with an aggregate literal.  
    // The type is determined by the context.  
    my\_point: Point \= { 5, 10 };  
      
    initial\_sum: i32 \= sum(my\_point); // Should be 15  
      
    move(\&my\_point); // my\_point is now {15, 10}  
      
    // The final result should be 15 \+ 15 \= 30  
    return my\_point.x \+ initial\_sum;  
}

### **Compile and Run**

\# Compile the program (use .csc or .c, the compiler doesn't mind)  
cargo run \-- tour.csc \-o tour\_app

\# Run the executable  
./tour\_app  
echo $? \# Should output 30

## **🔧 Installation**

### **Build from Source**

1. Ensure you have the [Rust toolchain](https://rustup.rs/) and clang installed.  
2. Clone this repository:  
   git clone https://github.com/karesis/cscript.git  
   cd cscript

3. Build the release executable:  
   cargo build \--release

   The executable will be located at target/release/cscript.

## **🗺️ The Road Ahead**

Our journey is just beginning\! The next steps are focused on expanding the language's power and ecosystem.

* \[ \] **Arrays & Slices**: Implement first-class support for fixed-size arrays and potentially dynamic slices.  
* \[ \] **Type Inference**: Introduce the let keyword for variable declarations, enabling inference like let x \= 10;.  
* \[ \] **Enums & Tuples**: Add more expressive data structures inspired by modern languages.  
* \[ \] **C FFI (Foreign Function Interface)**: Leverage our C-compatible data model to allow seamless interoperability with existing C libraries.  
* \[ \] **Module System**: Design and implement a simple file-based module system.  
* \[ \] **Optimization**: Integrate more of LLVM's powerful optimization passes.

## **🙌 Contributing**

CScript is an ambitious open-source project, and we welcome contributions of all kinds\! Whether it's reporting a bug, suggesting a feature, improving documentation, or writing code, your help is valued.  
Please feel free to open an issue or submit a pull request.

## **📜 License**

This project is licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](https://www.google.com/search?q=LICENSE-APACHE) or [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))  
* MIT license ([LICENSE-MIT](https://www.google.com/search?q=LICENSE-MIT) or [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))

at your option.