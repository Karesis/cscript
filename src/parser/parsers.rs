use crate::lexer::Token;
use super::ast::*;
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use crate::utils::Span;

// 为 chumsky 的标准错误类型 `extra::Err` 创建一个更简洁的别名。
pub(super) type ParseError<'a> = extra::Err<Rich<'a, Token, Span>>;

/// 构建并返回一个能解析整个程序的 chumsky 解析器。
///
/// 此函数是解析器模块的入口点，但其实现细节对外部模块隐藏。
pub(super) fn program_parser<'a, I>() -> impl Parser<'a, I, Program, ParseError<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    // --- 声明递归解析器 ---
    //
    // `expr` (表达式) 和 `block` (代码块) 存在相互引用的情况，
    // 例如，一个代码块包含表达式，而一个表达式（聚合字面量）又可以包含其他表达式。
    // 因此，我们使用 `Recursive::declare()` 创建“占位符”，稍后用 `.define()` 填充其定义。
    let mut expr = Recursive::declare();
    let mut block = Recursive::declare();

    // --- 原子及基础解析器 ---

    // 解析一个标识符 Token，并构造成 `Ident` AST 节点。
    // `select!` 宏用于从 Token 流中安全地匹配并提取特定枚举变体的数据。
    let ident = select! {
        Token::Ident(name) = e => Ident { name, span: e.span() }
    }
    .labelled("identifier");

    // 定义一个闭包来创建类型解析器。
    // 这允许我们在一个独立的逻辑单元中封装所有与类型解析相关的逻辑。
    let type_parser = || {
        // 首先，解析基础类型（如 i32, bool）或自定义结构体名（一个标识符）。
        let base_type = select! {
            Token::Int => Type::I32,
            Token::Char => Type::Char,
            Token::Bool => Type::Bool,
            Token::Void => Type::Void,
            Token::I8 => Type::I8,
            Token::I16 => Type::I16,
            Token::I32 => Type::I32,
            Token::I64 => Type::I64,
            Token::U8 => Type::U8,
            Token::U16 => Type::U16,
            Token::U32 => Type::U32,
            Token::U64 => Type::U64,
            Token::F32 => Type::F32,
            Token::F64 => Type::F64,
        }
        .or(ident.clone().map(Type::Struct));

        // 接着，在基础类型的基础上处理指针。
        // `then` 组合子会尝试匹配基础类型后的任意数量的 `*` 符号。
        base_type
            .then(
                just(Token::Star)
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            // 每匹配到一个 `*`，就将类型用 `Type::Ptr` 包裹一层。
            .map(|(mut base, pointers)| {
                for _ in pointers {
                    base = Type::Ptr(Box::new(base));
                }
                base
            })
            .labelled("type")
    };

    // 创建一个装箱的类型解析器实例，以便在递归和复杂场景中使用。
    let type_ = type_parser().boxed();

    // --- 表达式解析 (运算符优先级金字塔) ---
    //
    // 表达式的解析是解析器中最复杂的部分。我们采用“运算符优先级攀爬”的方法。
    // 基本思想是：为每个优先级定义一个解析器层级，高优先级的运算符在解析链的
    // 更底层被处理。每一层都在前一层（更高优先级）的基础上构建。
    //
    // 优先级由高到低排列如下：
    // 1. 原子 (Atom):              `123`, `"abc"`, `x`, `(expr)`, `{...}`
    // 2. 函数调用 (Call):          `f()`
    // 3. 成员访问 (Member Access): `obj.field`
    // 4. 一元运算 (Unary):         `-x`, `!x`, `&x`, `*x`
    // 5. 乘/除/模 (Product):       `*`, `/`, `%`
    // 6. 加/减 (Sum):              `+`, `-`
    // 7. 关系运算 (Relational):    `<`, `>`, `<=`, `>=`
    // 8. 相等运算 (Equality):      `==`, `!=`
    // 9. 逻辑与 (Logical AND):     `&&`
    // 10. 逻辑或 (Logical OR):     `||`
    // 11. 赋值 (Assignment):       `=`
    //
    expr.define({

        // 优先级 1: 原子表达式 (Atom) - 表达式的最基本单元。
        let atom = choice((
            // 聚合字面量, e.g., `{1, 2, 3}`
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with(|values, e| Expression {
                    kind: ExprKind::AggregateLiteral { values },
                    span: e.span(),
                }),
            // 各种基础类型的字面量。
            select! {
                Token::Integer(val) = e => Expression { kind: ExprKind::Literal(LiteralValue::Integer(val)), span: e.span() },
                Token::Float(val) = e => Expression { kind: ExprKind::Literal(LiteralValue::Float(val)), span: e.span() },
                Token::String(s) = e => Expression { kind: ExprKind::Literal(LiteralValue::String(s)), span: e.span() },
                Token::Boolean(b) = e => Expression { kind: ExprKind::Literal(LiteralValue::Bool(b)), span: e.span() },
            },
            // 变量。
            ident.clone().map(|ident| Expression { span: ident.span.clone(), kind: ExprKind::Variable(ident) }),
            // 带括号的表达式，括号提升了内部表达式的优先级。
            expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)),
        ));

        // 优先级 2: 函数调用。
        // 这一层解析器建立在原子表达式 `atom` 的基础上。它的任务是识别并构造
        // 函数调用表达式，例如 `my_function(arg1, 2, x + y)`。函数调用拥有比
        // 大多数运算符更高的优先级。
        let call = atom.clone()
            // 尝试在 `atom` 后面匹配一个由括号包裹的、逗号分隔的表达式列表。
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    // `.or_not()` 至关重要：它使得整个带括号的参数列表成为可选部分。
                    // 如果没有匹配到 `(...)`，它会返回 `None`；如果匹配到，则返回 `Some(Vec<Expression>)`。
                    .or_not(),
            )
            // `.map_with` 用于根据上面解析出的两部分（`callee` 和 `opt_call`）来构建最终的 AST 节点。
            .map_with(|(callee, opt_call), e| {
                // 首先，检查是否存在参数列表 (即 `opt_call` 是否为 `Some`)。
                if let Some(args) = opt_call {
                    // 如果确实解析到了一个参数列表 `(...)`，我们还需要确定被调用者 `callee` 的类型。
                    // 在我们的语言中，只有变量可以被调用，像 `(1+2)()` 或 `5()` 这样的表达式是不合法的函数调用。
                    //
                    // 这个内层的 `if let` 完美地处理了这种条件逻辑和所有权：
                    // - 如果 `callee.kind` 匹配 `ExprKind::Variable`，所有权被安全移出，用于构建新节点。
                    // - 如果不匹配，所有权不会移动，`callee` 在 `else` 分支中仍然可用。
                    if let ExprKind::Variable(name) = callee.kind {
                        // 条件满足：被调用者是变量，且后面跟着参数列表。
                        // 构造一个 `FunctionCall` AST 节点。
                        Expression { kind: ExprKind::FunctionCall { name, args }, span: e.span() }
                    } else {
                        // 虽然有参数列表，但被调用者不是一个有效的标识符（例如 `(1+2)()`）。
                        // 在这种情况下，我们将其视为一个普通的原子表达式，忽略后面的 `()` 部分。
                        callee
                    }
                } else {
                    // 如果 `opt_call` 是 `None`，意味着没有解析到参数列表 `(...)`。
                    // 这说明它就是一个普通的原子表达式，直接返回它即可。
                    callee
                }
            });

        // 优先级 3: 成员访问 (e.g., `my_struct.field`)。
        // 使用 `foldl` (左折叠) 来正确处理链式访问，如 `a.b.c` -> `(a.b).c`。
        let member_access = call.clone().foldl(
            // 重复匹配 `.member` 部分。
            just(Token::Dot).ignore_then(ident.clone()).repeated(),
            // 每匹配一次，就将左侧的表达式和右侧的成员组合成一个新的 `MemberAccess` 表达式。
            |expression, member| {
                let span = expression.span.clone().start..member.span.end;
                Expression {
                    kind: ExprKind::MemberAccess {
                        expression: Box::new(expression),
                        member,
                    },
                    span: span.into(),
                }
            },
        );

        // 辅助闭包，用于创建匹配单个 Token 的解析器。
        let op = |c| just(c);

        // 优先级 4: 一元运算符 (右结合)。
        let unary_op = op(Token::Minus).to(UnaryOp::Negate)
            .or(op(Token::Not).to(UnaryOp::Not))
            .or(op(Token::Ampersand).to(UnaryOp::AddressOf))
            .or(op(Token::Star).to(UnaryOp::Dereference));

        // 使用 `foldr` (右折叠) 来处理右结合性，如 `--x` -> `-(-x)`。
        let unary = unary_op.repeated().foldr(member_access, |op, right| {
            let span = right.span.clone(); // span of unary op should be same as right side
            Expression { kind: ExprKind::UnaryOp { op, right: Box::new(right) }, span }
        });

        // 以下是所有二元运算符的解析，它们都遵循相同的 `foldl` 模式以实现左结合性。

        // 优先级 5: 乘法/除法/取模 (左结合)。
        let product_op = op(Token::Star).to(BinaryOp::Multiply)
            .or(op(Token::Slash).to(BinaryOp::Divide))
            .or(op(Token::Percent).to(BinaryOp::Modulo));
        let product = unary.clone().foldl(product_op.then(unary).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span: span.into() }
        });

        // 优先级 6: 加法/减法 (左结合)。
        let sum_op = op(Token::Plus).to(BinaryOp::Add)
            .or(op(Token::Minus).to(BinaryOp::Subtract));
        let sum = product.clone().foldl(sum_op.then(product).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span: span.into() }
        });

        // 优先级 7: 关系运算 (左结合)。
        let relational_op = op(Token::Lt).to(BinaryOp::Lt)
            .or(op(Token::Lte).to(BinaryOp::Lte))
            .or(op(Token::Gt).to(BinaryOp::Gt))
            .or(op(Token::Gte).to(BinaryOp::Gte));
        let relation = sum.clone().foldl(relational_op.then(sum).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span: span.into() }
        });

        // 优先级 8: 相等运算 (左结合)。
        let equality_op = op(Token::Eq).to(BinaryOp::Eq)
            .or(op(Token::NotEq).to(BinaryOp::NotEq));
        let equality = relation.clone().foldl(equality_op.then(relation).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span: span.into() }
        });

        // 优先级 9: 逻辑与 (左结合)。
        let logical_and = equality.clone().foldl(op(Token::And).to(BinaryOp::And).then(equality).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span: span.into() }
        });

        // 优先级 10: 逻辑或 (左结合)。
        let logical_or = logical_and.clone().foldl(op(Token::Or).to(BinaryOp::Or).then(logical_and).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span: span.into() }
        });

        // 优先级 11: 赋值 (右结合，但通过 `.or_not()` 实现)。
        // 它的优先级最低，操作数是前面所有更高优先级的表达式。
        logical_or.clone().then(
            op(Token::Assign).to(()).then(expr.clone()).or_not()
        )
        .map_with(|(left, right_opt), e| {
            if let Some((_, right)) = right_opt {
                Expression { kind: ExprKind::Assignment { left: Box::new(left), right: Box::new(right) }, span: e.span() }
            } else {
                left
            }
        })
        .boxed()
        .labelled("expression")
    });

    // --- 语句与代码块解析 ---

    // 解析变量声明语句, e.g., `const x: i32 = 5;`
    let var_decl = just(Token::Const).or_not() // 可选的 'const'
        .then(ident.clone())                 // 变量名
        .then_ignore(just(Token::Colon))     // ':'
        .then(type_.clone())                 // 类型
        .then(just(Token::Assign).ignore_then(expr.clone()).or_not()) // 可选的初始化表达式
        .then_ignore(just(Token::Semicolon)) // ';'
        .map_with(|(((is_const, name), var_type), init), e| VarDecl {
            is_const: is_const.is_some(),
            var_type,
            name,
            init,
            span: e.span(),
        });

    // 解析单个语句。
    // 使用 `choice` 来尝试匹配所有可能的语句类型。
    let stmt = choice((
        // 代码块语句 `{ ... }`
        block.clone().map(Statement::Block),
        // return 语句 `return ...;`
        just(Token::Return)
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(Token::Semicolon))
            .map_with(|value, e| Statement::Return { value, span: e.span() }),
        // if-else 语句
        just(Token::If)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .map_with(|((condition, then_branch), else_branch), e| Statement::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
                span: e.span(),
            }),
        // while 循环语句
        just(Token::While)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(block.clone())
            .map_with(|(condition, body), e| Statement::While {
                condition,
                body: Box::new(body),
                span: e.span(),
            }),
        // break 和 continue 语句
        just(Token::Break).then_ignore(just(Token::Semicolon)).map_with(|_, e| Statement::Break(e.span())),
        just(Token::Continue).then_ignore(just(Token::Semicolon)).map_with(|_, e| Statement::Continue(e.span())),
        // 变量声明语句
        var_decl.clone().map(Statement::VarDecl),
        // 表达式语句 (e.g., a function call)
        expr.clone().then_ignore(just(Token::Semicolon)).map(Statement::Expr),
    ))
    .labelled("statement");

    // 定义代码块解析器。
    // 一个代码块由 `{` 和 `}` 包围的、零个或多个语句组成。
    block.define(
        stmt
            .repeated()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|stmts, e| Block { stmts, span: e.span() })
            .labelled("block")
    );

    // --- 顶层项解析 ---

    // --- 新增：解析 use 声明 (任务 1.2) ---
    // e.g., `use root::http::{Request, Response as Resp};`
    let use_decl_parser = {
        // 解析路径的第一部分，确定是 root, super, 还是相对路径
        let path_kind = choice((
            just(Token::Root).to(PathKind::Root),
            just(Token::Super).to(PathKind::Super),
        )).or_not() // .or_not() 使其成为可选的
           .map(|kind_opt| kind_opt.unwrap_or(PathKind::Relative));

        // 解析一个完整的路径, e.g., `root::http::server`
        let use_path_parser = path_kind
            .then(
                ident.clone()
                    .separated_by(just(Token::DoubleColon))
                    .at_least(1) // 路径至少要有一个部分
                    .collect::<Vec<_>>()
            )
            .map_with(|(kind, segments), e| UsePath { kind, segments, span: e.span() });

        // 解析嵌套 use 列表中的单个项, e.g., `Request` or `Response as Resp`
        let nested_use_item_parser = {
            // 解析 `Response as Resp`
            let renamed = ident.clone()
                .then_ignore(
                    // 注意：我们在这里直接匹配一个名为 "as" 的标识符 Token。
                    // 一个更健壮的设计可能是在 Lexer 中为 `as` 添加一个专门的 Token。
                    select! { Token::Ident(s) if s == "as" => () }
                )
                .then(ident.clone())
                .map(|(original, new_name)| NestedUseItem::Renamed { original, new_name });

            // 解析 `Request`
            let simple = ident.clone().map(|name| NestedUseItem::Simple { name });

            choice((renamed, simple))
        };
        
        // 解析 use 路径后面的目标部分
        let use_tree_parser = choice((
            // 匹配通配符 `*`
            just(Token::Star).to(UseTree::Glob),
            // 匹配嵌套列表 `{...}`
            nested_use_item_parser
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with(|items, e| UseTree::Nested { items, span: e.span() }),
        ));
        
        // 组合成完整的 use 声明解析器
        just(Token::Use)
            .ignore_then(use_path_parser)
            .then(
                // 路径后面的 `*` 或 `{...}` 是可选的
                use_tree_parser.or_not()
            )
            .then_ignore(just(Token::Semicolon))
            .map_with(|(path, target_opt), e| {
                // 如果没有匹配到 `*` 或 `{...}`，则默认为 Simple UseTree
                let target = target_opt.unwrap_or(UseTree::Simple);
                UseDecl { path, target, span: e.span() }
            })
            .labelled("use declaration")
    };

    // 解析结构体定义, e.g., `struct Point { x: i32, y: i32 }`
    let struct_def_parser = {
        // 解析单个字段, e.g., `x: i32`
        let field_parser = ident.clone()
            .then_ignore(just(Token::Colon))
            .then(type_.clone());

        // 解析结构体主体, e.g., `{ field1, field2, ... }`
        let body_parser = field_parser
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace));

        // 组合成完整的结构体定义解析器。
        just(Token::Struct)
            .ignore_then(ident.clone()) // struct <Name>
            .then(body_parser)          // { ... }
            .map_with(|(name, fields), e| StructDef { name, fields, span: e.span() })
            .labelled("struct definition")
    };

    // 解析 FFI (外部函数接口) 块, e.g., `extern "C" { ... }`
    let extern_block_parser = {
        // 解析单个函数参数 `name: type`，并转换为 AST 期望的 `(Type, Ident)` 元组。
        let param_decl = ident.clone()
            .then_ignore(just(Token::Colon))
            .then(type_.clone())
            .map(|(ident, type_)| (type_, ident));

        // 解析函数参数列表，能够健壮地处理普通、变长和空参数列表的所有情况。
        let params_parser = {
            // CASE A: 匹配一个或多个常规参数，其后可选地跟着 `, ...`
            let regular_params_with_optional_variadic = param_decl.clone()
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>()
                .then(
                    just(Token::Comma).ignore_then(just(Token::Ellipsis)).or_not()
                )
                .map(|(params, opt_ellipsis)| (params, opt_ellipsis.is_some()));

            // CASE B: 仅匹配 `...`
            let variadic_only = just(Token::Ellipsis).to((Vec::new(), true));

            // 将所有情况组合起来，并处理空参数列表 `()` 的情况。
            choice((
                regular_params_with_optional_variadic,
                variadic_only,
            ))
            .or_not() // `or_not` 使得整个参数列表可以是空的。
            .map(|opt| opt.unwrap_or_else(|| (Vec::new(), false))) // 为 `None` (即空参数) 提供默认值。
        };

        // 解析单条外部函数声明 `name(...) -> type;`
        let function_decl_parser = ident.clone()
            .then(
                params_parser
                    .delimited_by(just(Token::LParen), just(Token::RParen))
            )
            .then(just(Token::Arrow).ignore_then(type_.clone()))
            .then_ignore(just(Token::Semicolon))
            .map_with(|((name, (params, is_variadic)), return_type), e| FunctionDecl {
                name,
                params,
                return_type,
                is_variadic,
                span: e.span(),
            });

        // 组合成完整的 `extern "ABI" { ... }` 解析器。
        just(Token::Extern)
            .ignore_then(select! { Token::String(s) => s })
            .then(
                function_decl_parser
                    .repeated()
                    .collect()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
            )
            .map_with(|(abi, declarations), e| ExternBlock { abi, declarations, span: e.span() })
            .labelled("extern block")
    };

    // 解析函数定义。
    let func_def = ident.clone() // 1. 函数名
        .then( // 2. 参数列表
            ident.clone()
                .then_ignore(just(Token::Colon))
                .then(type_.clone())
                // `map` 用于将解析结果 (Ident, Type) 转换为 AST 期望的 (Type, Ident)。
                .map(|(ident, type_)| (type_, ident))
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then( // 3. 返回类型
            just(Token::Arrow)
                .ignore_then(type_.clone())
        )
        .then(block.clone()) // 4. 函数体
        .map_with(|(((name, params), return_type), body), e| {
            GlobalItem::Function(FunctionDef {
                return_type,
                name,
                params,
                body,
                span: e.span(),
            })
        })
        .labelled("function definition");

    // --- 最终组装 ---

    // `item` 解析器代表任何一个顶层定义。
    let item = choice((
        use_decl_parser.map(GlobalItem::Use), 
        extern_block_parser.map(GlobalItem::Extern),
        struct_def_parser.map(GlobalItem::Struct),
        func_def,
        var_decl.map(GlobalItem::VarDecl),
    )).boxed();

    // 整个程序是由零个或多个顶层项组成的。
    item.repeated()
        .collect()
        .map_with(|items, e| Program { items, span: e.span() })
        // `then_ignore(end())` 确保我们已经解析了所有 Token，
        // 如果有多余的 Token，解析将会失败，这保证了整个输入的有效性。
        .then_ignore(end())
}