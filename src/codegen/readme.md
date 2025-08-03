src/
└── codegen/
    ├── mod.rs          # 定义 CodeGenCtx, GenerateHIR trait, 和顶层 run 函数
    ├── expression.rs   # impl GenerateHIR for hir::Expression 和相关类型
    ├── statement.rs    # impl GenerateHIR for hir::Statement 和相关类型
    ├── function.rs     # 函数声明、函数体的生成逻辑
    ├── globals.rs      # 全局变量的生成逻辑
    └── lvalue.rs       # lvalue 的生成逻辑
    └── utils.rs        # to_llvm_type, create_entry_block_alloca 等辅助函数