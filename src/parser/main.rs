//! CScript 语言的解析器实现。
//! 该版本采用递归下降和普拉特解析（用于表达式）的技术，
//! 将 Token 流转换为抽象语法树 (AST)。

use super::ast::*; 
use crate::diagnostic::*;
use crate::lexer::*;

// --- 1. 主解析器结构体 ---

/// 解析器结构体，持有解析过程所需的全部状态。
pub struct Parser<'a> {
    /// 从 Lexer 获取的 Token 流的引用。
    tokens: &'a [Token],
    /// 诊断信息收集器，用于报告所有解析错误。
    diagnostics: &'a mut DiagnosticBag,
    /// 指向当前待处理 Token 的指针（在 tokens 切片中的索引）。
    current: usize,
}

// --- 2. Trait 定义：用于组织解析逻辑 ---

/// `Parse` Trait 是解析器的总入口。
pub trait Parse {
    /// 消耗解析器并启动整个解析过程，最终生成一个 `Program` AST 节点。
    fn parse(self) -> Program;
}

/// `DeclarationParser` Trait 负责解析顶层声明。
trait DeclarationParser {
    /// 解析一个顶层声明（函数或全局变量）。
    fn parse_declaration(&mut self) -> Result<Node<Declaration>, ()>;
    /// 解析一个函数定义。
    fn parse_function_definition(&mut self) -> Result<Node<FunctionDefinition>, ()>;
    /// 解析函数参数列表。
    fn parse_parameter_list(&mut self) -> Result<Vec<Node<Parameter>>, ()>;
}

/// `StatementParser` Trait 负责解析各类语句。
trait StatementParser {
    /// 解析任意类型的语句。
    fn parse_statement(&mut self) -> Result<Node<Statement>, ()>;
    /// 解析一个变量声明语句（也用于全局变量）。
    fn parse_variable_declaration(&mut self) -> Result<Node<VariableDeclaration>, ()>;
    /// 解析 `if` 语句。
    fn parse_if_statement(&mut self) -> Result<Node<IfStatement>, ()>;
    /// 解析 `while` 语句。
    fn parse_while_statement(&mut self) -> Result<Node<WhileStatement>, ()>;
    /// 解析 `return` 语句。
    fn parse_return_statement(&mut self) -> Result<Node<ReturnStatement>, ()>;
    /// 解析一个 `{...}` 代码块。
    fn parse_block_statement(&mut self) -> Result<Node<BlockStatement>, ()>;
}

/// `ExpressionParser` Trait 负责解析各类表达式（使用普拉特解析）。
trait ExpressionParser {
    /// 解析表达式的主入口（普拉特解析）。
    fn parse_expression(&mut self, min_bp: u8) -> Result<Node<Expression>, ()>;
    /// 解析前缀表达式（例如 `-x`, `!flag`）。
    fn parse_prefix_expression(&mut self) -> Result<Node<Expression>, ()>;
    /// 解析中缀表达式（例如 `a + b`）。
    fn parse_infix_expression(&mut self, left: Node<Expression>) -> Result<Node<Expression>, ()>;
    /// 解析后缀表达式（例如函数调用 `f()`）。
    fn parse_postfix_expression(&mut self, left: Node<Expression>) -> Result<Node<Expression>, ()>;
    /// 解析原子表达式（表达式的最基本单元，如字面量、标识符、括号表达式）。
    fn parse_atom(&mut self) -> Result<Node<Expression>, ()>;
    /// 解析函数调用的参数列表。
    fn parse_call_arguments(&mut self) -> Result<Vec<Node<Expression>>, ()>;
}

/// `TypeParser` Trait 负责解析类型规范。
trait TypeParser {
    /// 解析一个类型规范（例如 `int`, `char*`, `int**`）。
    fn parse_type_spec(&mut self) -> Result<Node<TypeSpec>, ()>;
}

/// `Util` Trait 提供了解析过程中常用的一系列辅助函数。
trait Util {
    // --- Token 流操作 ---
    /// 查看当前的 Token。
    fn peek(&self) -> &Token;
    /// 查看下一个 Token。
    fn peek_next(&self) -> &Token;
    /// 获取前一个刚刚被消费的 Token。
    fn previous(&self) -> &Token;
    /// 检查是否已到达 Token 流的末尾。
    fn is_at_end(&self) -> bool;
    /// 消费当前 Token 并返回它，同时前移指针。
    fn advance(&mut self) -> &Token;
    /// 检查当前 Token 是否是指定的类型。
    fn check(&self, kind: &TokenKind) -> bool;
    /// 如果当前 Token 是指定类型，则消费它并返回 `true`。
    fn match_token(&mut self, kind: &TokenKind) -> bool;
    /// 消费一个指定类型的 Token，如果不是预期类型则报告错误。
    fn consume(&mut self, kind: &TokenKind, message: &str) -> Result<&Token, ()>;
    /// 消费一个预期的分号 `;`，如果缺失则报告错误。
    fn consume_semicolon(&mut self, message: &str) -> Result<(), ()>;

    // --- 错误恢复 ---
    /// 同步，用于错误恢复。当解析出错时，尝试丢弃 Token 直到一个安全的“同步点”，以便继续解析。
    fn synchronize(&mut self);

    // --- 普拉特解析辅助函数 ---
    /// 获取前缀运算符的绑定力。
    fn prefix_binding_power(kind: &TokenKind) -> Option<((), u8)>;
    /// 获取中缀运算符的绑定力。
    fn infix_binding_power(kind: &TokenKind) -> Option<(u8, u8)>;
    /// 获取后缀运算符的绑定力。
    fn postfix_binding_power(kind: &TokenKind) -> Option<(u8, ())>;
}


// --- 3. 基础实现 ---

impl<'a> Parser<'a> {
    /// 创建一个新的解析器实例。
    pub fn new(tokens: &'a [Token], diagnostics: &'a mut DiagnosticBag) -> Self {
        Parser {
            tokens,
            diagnostics,
            current: 0,
        }
    }
}

// 这里是解析器的总入口。
// 我们之后将为 `Parser` 实现上面定义的所有 trait。
impl<'a> Parse for Parser<'a> {
    fn parse(mut self) -> Program {
        let mut declarations = Vec::new();
        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(decl) => declarations.push(decl),
                Err(_) => self.synchronize(),
            }
        }
        Program { declarations }
    }
}

impl<'a> DeclarationParser for Parser<'a> {
    /// 解析一个顶层声明。
    ///
    /// 这是一个分发函数。C 语言的函数定义和全局变量声明都以 "类型 + 标识符" 开头，
    /// 例如 `int x`。为了区分它们，我们需要“向前看”（lookahead）一个 Token。
    /// 如果标识符后面是 `(`, 那么它是一个函数定义。
    /// 否则，我们假定它是一个变量声明。
    fn parse_declaration(&mut self) -> Result<Node<Declaration>, ()> {
        // --- 执行前瞻（Lookahead）来决定路径 ---
        // 我们通过一个不改变解析器状态的“窥视”来决定。
        // 1. 我们先跳过类型部分（例如 int, char, *, 等）
        let mut lookahead = self.current;
        while let Some(token) = self.tokens.get(lookahead) {
            match token.kind {
                // 这些都是类型的一部分，继续向后看
                TokenKind::Keyword(Keyword::Int | Keyword::Char | Keyword::Void | Keyword::Bool)
                | TokenKind::Operator(Operator::Star) => {
                    lookahead += 1;
                }
                _ => break,
            }
        }
        // 2. 跳过标识符
        if let Some(token) = self.tokens.get(lookahead) {
            if matches!(token.kind, TokenKind::Identifier(_)) {
                lookahead += 1;
            }
        }
        // 3. 检查标识符后面的 Token
        let is_function = if let Some(token) = self.tokens.get(lookahead) {
            matches!(token.kind, TokenKind::Punctuation(Punctuation::LParen))
        } else {
            false
        };
        // --- 前瞻结束 ---

        if is_function {
            // 如果是函数，调用函数定义解析器
            self.parse_function_definition()
                .map(|node| node.map(Declaration::Function))
        } else {
            // 否则，调用变量声明解析器
            // 注意：parse_variable_declaration 定义在 StatementParser trait 中，
            // 但我们在这里调用它来处理全局变量，然后包装成顶层 Declaration。
            let var_node = self.parse_variable_declaration()?;
            // 全局变量声明必须以分号结尾
            self.consume_semicolon("全局变量声明后需要一个分号 `;`")?;
            Ok(var_node.map(Declaration::Variable))
        }
    }

    /// 解析一个完整的函数定义。
    /// 例如: `int add(int a, int b) { ... }`
    fn parse_function_definition(&mut self) -> Result<Node<FunctionDefinition>, ()> {
        // 1. 解析返回类型
        let return_type = self.parse_type_spec()?;
        let start_span = return_type.span;

        // 2. 解析函数名
        let name_token = self.peek().clone();
        let name = if let TokenKind::Identifier(n) = &name_token.kind {
            self.advance();
            Identifier { name: n.clone(), span: name_token.span }
        } else {
            let label = Label::new(name_token.span, "这里需要一个函数名");
            self.diagnostics.report_error("预期的函数名", label);
            return Err(());
        };

        // 3. 解析参数列表
        self.consume(&TokenKind::Punctuation(Punctuation::LParen), "函数名后需要一个 `(`")?;
        let params = self.parse_parameter_list()?;
        self.consume(&TokenKind::Punctuation(Punctuation::RParen), "参数列表后需要一个 `)`")?;

        // 4. 解析函数体
        let body = self.parse_block_statement()?;
        let end_span = body.span;

        // 5. 构建并返回 FunctionDefinition 节点
        let func_def = FunctionDefinition { name, params, return_type, body };
        Ok(Node { kind: func_def, span: start_span.to(end_span) })
    }

    /// 解析函数定义中的参数列表。
    /// 例如: `()` 或 `(int a, char* b)` 或 `(void)`
    fn parse_parameter_list(&mut self) -> Result<Vec<Node<Parameter>>, ()> {
        let mut params = Vec::new();

        // 处理空参数列表 `()` 的情况
        if self.check(&TokenKind::Punctuation(Punctuation::RParen)) {
            return Ok(params);
        }

        // 处理 C 语言特有的 `(void)` 空参数列表
        if self.check(&TokenKind::Keyword(Keyword::Void)) && self.peek_next().kind == TokenKind::Punctuation(Punctuation::RParen) {
            self.advance(); // 消费 'void'
            return Ok(params);
        }

        // 循环解析每个参数
        loop {
            // 1. 解析参数类型
            let type_spec = self.parse_type_spec()?;
            let start_span = type_spec.span;

            // 2. 解析参数名
            let name_token = self.peek().clone();
            let name = if let TokenKind::Identifier(n) = &name_token.kind {
                self.advance();
                Identifier { name: n.clone(), span: name_token.span }
            } else {
                let label = Label::new(name_token.span, "这里需要一个参数名");
                self.diagnostics.report_error("预期的参数名", label);
                return Err(());
            };
            let end_span = name.span;

            // 3. 构建并存储参数节点
            params.push(Node {
                kind: Parameter { name, type_spec },
                span: start_span.to(end_span),
            });

            // 4. 检查是否还有下一个参数（由逗号分隔）
            if !self.match_token(&TokenKind::Punctuation(Punctuation::Comma)) {
                break; // 如果没有逗号，说明参数列表结束
            }
        }

        Ok(params)
    }
}

impl<'a> StatementParser for Parser<'a> {
    /// 解析任意类型的语句，这是语句解析的总入口。
    ///
    /// 它通过“向前看”第一个 Token 来决定应该调用哪个更具体的解析函数。
    fn parse_statement(&mut self) -> Result<Node<Statement>, ()> {
        // 根据当前 Token 的类型，分发到不同的语句解析函数
        let statement_node = match self.peek().kind {
            TokenKind::Keyword(Keyword::If) => {
                return self.parse_if_statement().map(|n| n.map(Statement::If));
            }
            TokenKind::Keyword(Keyword::While) => {
                return self.parse_while_statement().map(|n| n.map(Statement::While));
            }
            TokenKind::Keyword(Keyword::Return) => {
                let node = self.parse_return_statement()?.map(Statement::Return);
                self.consume_semicolon("return 语句后需要一个分号 ';'")?;
                return Ok(node);
            }
            TokenKind::Punctuation(Punctuation::LBrace) => {
                return self.parse_block_statement().map(|n| n.map(Statement::Block));
            }
            // 如果 Token 是一个类型关键字，我们认为它是一个变量声明
            TokenKind::Keyword(kw) if matches!(kw, Keyword::Int | Keyword::Char | Keyword::Void | Keyword::Bool) => {
                let node = self.parse_variable_declaration()?.map(Statement::VariableDeclaration);
                self.consume_semicolon("变量声明后需要一个分号 ';'")?;
                return Ok(node);
            }
            // 如果以上都不是，那么它只能是一个表达式语句
            _ => {
                let expr_node = self.parse_expression(0)?;
                let span = expr_node.span; // 先保存 span
                // 构造一个 `Node<Statement>`
                let node = Node {
                    kind: Statement::Expression(expr_node),
                    span,
                };
                self.consume_semicolon("表达式语句后需要一个分号 ';'")?;
                return Ok(node);
            }
        };
    }

    /// 解析一个变量声明语句。
    /// 例如: `int x;` 或 `char* buffer = "hello";`
    fn parse_variable_declaration(&mut self) -> Result<Node<VariableDeclaration>, ()> {
        // 1. 解析类型
        let type_spec = self.parse_type_spec()?;
        let start_span = type_spec.span;

        // 2. 解析变量名
        let name_token = self.peek().clone();
        let name = if let TokenKind::Identifier(n) = &name_token.kind {
            self.advance();
            Identifier { name: n.clone(), span: name_token.span }
        } else {
            let label = Label::new(name_token.span, "这里需要一个变量名");
            self.diagnostics.report_error_with_code("预期的变量名", ErrorCode::E1003, label);
            return Err(());
        };

        // 3. 解析初始化表达式
        let initializer = if self.match_token(&TokenKind::Operator(Operator::Assign)) {
            Some(self.parse_expression(0)?)
        } else {
            None
        };
        
        let end_span = initializer.as_ref().map_or(name.span, |i| i.span);

        // 4. 构建并返回节点
        Ok(Node {
            kind: VariableDeclaration { name, type_spec, initializer },
            span: start_span.to(end_span),
        })
    }

    /// 解析 `if` 语句。
    /// 例如: `if (x > 0) { ... } else { ... }`
    fn parse_if_statement(&mut self) -> Result<Node<IfStatement>, ()> {
        // 1. 消费 `if` 关键字
        let start_span = self.consume(&TokenKind::Keyword(Keyword::If), "期望 `if` 关键字")?.span;
        
        // 2. 解析条件表达式
        self.consume(&TokenKind::Punctuation(Punctuation::LParen), "if 后需要一个 `(`")?;
        let condition = self.parse_expression(0)?;
        self.consume(&TokenKind::Punctuation(Punctuation::RParen), "if 条件后需要一个 `)`")?;
        
        // 3. 解析 `then` 分支的主体语句
        let then_block = Box::new(self.parse_statement()?);
        let mut end_span = then_block.span;

        // 4. (可选) 解析 `else` 分支
        let else_branch = if self.match_token(&TokenKind::Keyword(Keyword::Else)) {
            let else_node = Box::new(self.parse_statement()?);
            end_span = else_node.span;
            Some(else_node)
        } else {
            None
        };

        // 5. 构建并返回节点
        Ok(Node {
            kind: IfStatement { condition, then_block, else_branch },
            span: start_span.to(end_span),
        })
    }

    /// 解析 `while` 语句。
    /// 例如: `while (i < 10) { ... }`
    fn parse_while_statement(&mut self) -> Result<Node<WhileStatement>, ()> {
        // 1. 消费 `while` 关键字
        let start_span = self.consume(&TokenKind::Keyword(Keyword::While), "期望 `while` 关键字")?.span;
        
        // 2. 解析条件表达式
        self.consume(&TokenKind::Punctuation(Punctuation::LParen), "while 后需要一个 `(`")?;
        let condition = self.parse_expression(0)?;
        self.consume(&TokenKind::Punctuation(Punctuation::RParen), "while 条件后需要一个 `)`")?;
        
        // 3. 解析循环体语句
        let body = Box::new(self.parse_statement()?);
        let end_span = body.span;

        // 4. 构建并返回节点
        Ok(Node {
            kind: WhileStatement { condition, body },
            span: start_span.to(end_span),
        })
    }

    /// 解析 `return` 语句。
    /// 例如: `return;` 或 `return 0;`
    fn parse_return_statement(&mut self) -> Result<Node<ReturnStatement>, ()> {
        // 1. 消费 `return` 关键字
        let start_span = self.consume(&TokenKind::Keyword(Keyword::Return), "期望 `return` 关键字")?.span;

        // 2. 解析返回值表达式
        let value = if self.check(&TokenKind::Punctuation(Punctuation::Semicolon)) {
            // 如果 `return` 后面直接跟分号，则没有返回值
            None
        } else {
            Some(self.parse_expression(0)?)
        };
        
        let end_span = value.as_ref().map_or(start_span, |v| v.span);

        // 3. 构建并返回节点
        Ok(Node {
            kind: ReturnStatement { value },
            span: start_span.to(end_span),
        })
    }

    /// 解析一个 `{...}` 代码块语句。
    fn parse_block_statement(&mut self) -> Result<Node<BlockStatement>, ()> {
        // 1. 消费 `{`
        let start_span = self.consume(&TokenKind::Punctuation(Punctuation::LBrace), "期望 `{` 来开始一个代码块")?.span;
        
        let mut statements = Vec::new();
        // 2. 循环解析代码块中的每一条语句
        while !self.check(&TokenKind::Punctuation(Punctuation::RBrace)) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        
        // 3. 消费 `}`
        let end_span = self.consume(&TokenKind::Punctuation(Punctuation::RBrace), "期望 `}` 来结束一个代码块")?.span;

        // 4. 构建并返回节点
        Ok(Node {
            kind: BlockStatement { statements },
            span: start_span.to(end_span),
        })
    }
}

impl<'a> ExpressionParser for Parser<'a> {
    /// 解析表达式的主入口函数，实现了普拉特解析的核心循环。
    fn parse_expression(&mut self, min_bp: u8) -> Result<Node<Expression>, ()> {
        // 1. 每个表达式都必须由一个前缀部分开始（原子或前缀运算符）。
        let mut left = self.parse_prefix_expression()?;

        // 2. 循环处理所有优先级高于 min_bp 的中缀和后缀运算符。
        loop {
            let current_token = self.peek();

            // 检查后缀运算符（函数调用）
            if let Some((l_bp, ())) = Self::postfix_binding_power(&current_token.kind) {
                if l_bp < min_bp {
                    break;
                }
                left = self.parse_postfix_expression(left)?;
                continue;
            }

            // 检查中缀运算符
            if let Some((l_bp, _)) = Self::infix_binding_power(&current_token.kind) {
                if l_bp < min_bp {
                    break;
                }
                left = self.parse_infix_expression(left)?;
                continue;
            }

            break; // 没有更高优先级的运算符了，循环结束。
        }

        Ok(left)
    }
    
    /// 解析前缀表达式，如 `-x`, `!flag`, `&var`, `*ptr`。
    fn parse_prefix_expression(&mut self) -> Result<Node<Expression>, ()> {
        // 检查当前 Token 是否是定义的前缀运算符
        if let Some(((), r_bp)) = Self::prefix_binding_power(&self.peek().kind) {
            let op_token = self.advance().clone();
            let op = if let TokenKind::Operator(o) = op_token.kind { o } else { unreachable!() };
            
            // 递归调用 parse_expression 来解析运算符右边的操作数
            let operand = self.parse_expression(r_bp)?;
            let span = op_token.span.to(operand.span);

            let kind = Expression::Unary(UnaryExpression {
                operator: Node { kind: op, span: op_token.span },
                operand: Box::new(operand),
            });
            Ok(Node { kind, span })
        } else {
            // 如果不是前缀运算符，那它一定是一个“原子”表达式。
            self.parse_atom()
        }
    }
    
    /// 解析原子表达式，即表达式的最小组成单元。
    fn parse_atom(&mut self) -> Result<Node<Expression>, ()> {
        let token = self.advance().clone();
        match token.kind {
            // 字面量，如 123, 'a'
            TokenKind::Literal(lit) => Ok(Node {
                kind: Expression::Literal(lit),
                span: token.span,
            }),
            // 标识符，如变量名
            TokenKind::Identifier(name) => Ok(Node {
                kind: Expression::Identifier(Identifier { name, span: token.span }),
                span: token.span,
            }),
            // 括号表达式 `(...)`
            TokenKind::Punctuation(Punctuation::LParen) => {
                let start_span = token.span;
                let expr = self.parse_expression(0)?; // 括号内表达式的优先级最低
                let end_span = self.consume(&TokenKind::Punctuation(Punctuation::RParen), "期望一个 `)` 来闭合括号表达式")?.span;
                // 注意：我们直接返回内部的表达式节点，但可以根据需要调整
                Ok(Node { kind: expr.kind, span: start_span.to(end_span) })
            }
            // 其他情况都是非法的表达式开头
            _ => {
                let label = Label::new(token.span, "这个符号不能作为表达式的开头");
                self.diagnostics.report_error_with_code("期望一个表达式", ErrorCode::E1005, label);
                Err(())
            }
        }
    }
    
    /// 解析中缀表达式，如 `a + b`, `x = 10`。
    fn parse_infix_expression(&mut self, left: Node<Expression>) -> Result<Node<Expression>, ()> {
        let op_token = self.advance().clone();
        let op = if let TokenKind::Operator(o) = op_token.kind { o } else { unreachable!() };
        let (_, r_bp) = Self::infix_binding_power(&op_token.kind).unwrap();

        // 递归调用来解析右操作数
        let right = self.parse_expression(r_bp)?;
        let span = left.span.to(right.span);

        // 根据是赋值还是其他二元运算，创建不同的 AST 节点
        let kind = if op == Operator::Assign {
            Expression::Assignment(AssignmentExpression {
                lvalue: Box::new(left),
                rvalue: Box::new(right),
            })
        } else {
            Expression::Binary(BinaryExpression {
                left: Box::new(left),
                operator: Node { kind: op, span: op_token.span },
                right: Box::new(right),
            })
        };

        Ok(Node { kind, span })
    }

    /// 解析后缀表达式，在 `c0` 中仅有关函数调用 `f()`。
    fn parse_postfix_expression(&mut self, left: Node<Expression>) -> Result<Node<Expression>, ()> {
        // 后缀表达式总是函数调用
        let arguments = self.parse_call_arguments()?;
        let end_span = self.previous().span; // ')' 的位置
        let span = left.span.to(end_span);

        let kind = Expression::Call(CallExpression {
            callee: Box::new(left),
            arguments,
        });
        Ok(Node { kind, span })
    }

    /// 解析函数调用的参数列表 `(arg1, arg2, ...)`。
    fn parse_call_arguments(&mut self) -> Result<Vec<Node<Expression>>, ()> {
        // 消费 `(`
        self.consume(&TokenKind::Punctuation(Punctuation::LParen), "函数调用期望一个 `(`")?;
        
        let mut args = Vec::new();
        if self.check(&TokenKind::Punctuation(Punctuation::RParen)) {
            // 空参数列表 `()`
            self.advance();
            return Ok(args);
        }

        // 循环解析由逗号分隔的参数表达式
        loop {
            args.push(self.parse_expression(0)?);
            if !self.match_token(&TokenKind::Punctuation(Punctuation::Comma)) {
                break;
            }
        }

        self.consume(&TokenKind::Punctuation(Punctuation::RParen), "参数列表后需要一个 `)`")?;
        Ok(args)
    }
}

impl<'a> TypeParser for Parser<'a> {
    /// 解析一个类型规范。
    ///
    /// 这个函数首先解析一个基础类型，然后循环地解析跟随其后的所有指针 `*`，
    /// 将它们像洋葱一样层层包裹起来，最终形成完整的类型 AST 节点。
    /// 例如，对于 `int**`，它会构建出 `Pointer(Pointer(Named("int")))` 这样的结构。
    fn parse_type_spec(&mut self) -> Result<Node<TypeSpec>, ()> {
        // 1. 解析基础类型 (int, char, void, bool)
        let base_token = self.peek().clone();
        let mut type_node = match base_token.kind {
            TokenKind::Keyword(kw) => match kw {
                // 确认关键字是一个类型说明符
                Keyword::Int | Keyword::Char | Keyword::Void | Keyword::Bool => {
                    self.advance(); // 消费类型关键字
                    let ident = Identifier {
                        name: kw.to_string(),
                        span: base_token.span,
                    };
                    Node {
                        kind: TypeSpec::Named(ident),
                        span: base_token.span,
                    }
                }
                _ => {
                    // 如果是其他关键字，则不是合法的类型开头
                    let label = Label::new(base_token.span, "这个关键字不能作为类型的开头");
                    self.diagnostics.report_error_with_code("期望一个类型说明符", ErrorCode::E1004, label);
                    return Err(());
                }
            },
            _ => {
                // 如果不是关键字，也不是合法的类型开头
                let label = Label::new(base_token.span, "只有 `int`, `char`, `void`, `bool` 可以作为类型的开头");
                self.diagnostics.report_error_with_code("期望一个类型说明符", ErrorCode::E1004, label);
                return Err(());
            }
        };

        // 2. 循环解析指针 `*`
        // `while self.match_token(...)` 是一个非常惯用的模式，
        // 它会持续消费 `*` Token，直到找不到为止。
        while self.match_token(&TokenKind::Operator(Operator::Star)) {
            // `self.previous()` 获取我们刚刚消费掉的 `*` Token
            let star_token = self.previous();
            // 更新 span 以包含新消费的 `*`
            let new_span = type_node.span.to(star_token.span);
            
            // 将当前的类型节点用一个新的 `Pointer` 节点包裹起来
            type_node = Node {
                kind: TypeSpec::Pointer(Box::new(type_node)),
                span: new_span,
            };
        }

        // 3. 返回最终构建完成的类型节点
        Ok(type_node)
    }
}

impl<'a> Util for Parser<'a> {
    // --- Token 流操作 ---

    /// 查看当前的 Token，但并不消费它。
    /// 如果已经到达文件末尾，则稳定地返回 EOF Token，防止越界。
    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current)
            .unwrap_or_else(|| &self.tokens[self.tokens.len() - 1]) // 安全地返回最后的 EOF Token
    }

    /// 查看下一个 Token。
    fn peek_next(&self) -> &Token {
        self.tokens
            .get(self.current + 1)
            .unwrap_or_else(|| &self.tokens[self.tokens.len() - 1])
    }

    /// 获取前一个刚刚被消费的 Token。
    /// 这个函数总是在 `advance()` 之后被调用，因此 `self.current - 1` 是安全的。
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    /// 检查是否已到达 Token 流的末尾（即当前 Token 是否是 EOF）。
    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    /// 消费当前 Token 并返回它，同时将指针前移一位。
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    /// 检查当前 Token 的类型是否是所期望的类型。
    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().kind == kind
    }

    /// 如果当前 Token 是指定类型，则消费它并返回 `true`，否则返回 `false`。
    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// 消费一个指定类型的 Token。如果当前 Token 不是预期类型，则报告一个错误。
    fn consume(&mut self, kind: &TokenKind, message: &str) -> Result<&Token, ()> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            let found = self.peek();
            let label_msg = format!("期望得到 {}，但实际得到 {}", kind.to_string_for_error(), found.kind.to_string_for_error());
            let label = Label::new(found.span, label_msg);
            self.diagnostics.report_error_with_code(message, ErrorCode::E1001, label);
            Err(())
        }
    }

    /// `consume` 的一个特化版本，用于消费语句结尾的分号。
    fn consume_semicolon(&mut self, message: &str) -> Result<(), ()> {
        self.consume(&TokenKind::Punctuation(Punctuation::Semicolon), message)
            .map(|_| ()) // 将 Result<&Token, ()> 转换为 Result<(), ()>
    }

    // --- 错误恢复 ---

    /// 同步函数，用于解析器从错误中恢复。
    /// 当遇到语法错误时，我们不想立即停止，而是尝试跳过一些 Token，
    /// 直到找到一个可以安全地重新开始解析的地方（例如下一条语句的开头）。
    fn synchronize(&mut self) {
        self.advance(); // 跳过导致错误的 Token

        while !self.is_at_end() {
            // 如果前一个 Token 是分号，那么我们很可能位于一条新语句的开头，可以安全退出。
            if self.previous().kind == TokenKind::Punctuation(Punctuation::Semicolon) {
                return;
            }

            // 如果下一个 Token 是一个可以开始新声明或新语句的关键字，也可以安全退出。
            match self.peek().kind {
                TokenKind::Keyword(kw) => match kw {
                    Keyword::Int | Keyword::Char | Keyword::Void | Keyword::Bool |
                    Keyword::If | Keyword::While | Keyword::Return => return,
                    _ => (),
                },
                _ => (),
            }

            self.advance();
        }
    }

    // --- 普拉特解析辅助函数 ---
    
    /// 获取前缀运算符的绑定力。
    fn prefix_binding_power(kind: &TokenKind) -> Option<((), u8)> {
        let bp = match kind {
            TokenKind::Operator(op) => match op {
                Operator::Minus | Operator::Not | Operator::And | Operator::Star => 15, // -, !, &, *
                _ => return None,
            },
            _ => return None,
        };
        Some(((), bp))
    }

    /// 获取中缀运算符的绑定力。
    fn infix_binding_power(kind: &TokenKind) -> Option<(u8, u8)> {
        let bp = match kind {
            TokenKind::Operator(op) => match op {
                Operator::Assign => (2, 1),
                Operator::OrOr => (3, 4),
                Operator::AndAnd => (5, 6),
                Operator::Eq | Operator::NotEq => (7, 8),
                Operator::Lt | Operator::Gt | Operator::LtEq | Operator::GtEq => (9, 10),
                Operator::Plus | Operator::Minus => (11, 12),
                Operator::Star | Operator::Slash | Operator::Percent => (13, 14),
                _ => return None,
            },
            _ => return None,
        };
        Some(bp)
    }

    /// 获取后缀运算符的绑定力。
    fn postfix_binding_power(kind: &TokenKind) -> Option<(u8, ())> {
        let bp = match kind {
            TokenKind::Punctuation(Punctuation::LParen) => 16, // 函数调用
            _ => return None,
        };
        Some((bp, ()))
    }
}