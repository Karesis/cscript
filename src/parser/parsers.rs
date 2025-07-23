// src/parser/parsers.rs

use crate::lexer::{Span, Token};
use crate::parser::ast::*;
use chumsky::input::ValueInput;
use chumsky::prelude::*;

// [CORRECTED] 这是标准的、正确的 chumsky 错误类型别名。
pub(super) type ParseError<'a> = extra::Err<Rich<'a, Token, Span>>;

/// 构建完整的 chumsky 解析器。
/// 此函数为内部实现细节，仅对父模块 `mod.rs` 可见。
pub(super) fn program_parser<'a, I>() -> impl Parser<'a, I, Program, ParseError<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    // --- 递归解析器声明 ---
    let mut expr = Recursive::declare();
    let mut block = Recursive::declare();

    // --- 基础解析器 ---
    let ident = select! { Token::Ident(name) = e => Ident { name, span: e.span() } }.labelled("identifier");
    let type_ = type_parser().boxed();

    // --- 表达式解析器定义 ---
    expr.define({
        // [CORRECTED] 彻底修复了 .unwrap() 问题。
        // 因为我们的词法分析器已经将整数解析为 i64，这里不再需要进行任何 parse() 调用。
        let atom = choice((
            select! {
                Token::Integer(val) = e => Expression { kind: ExprKind::Literal(LiteralValue::Integer(val)), span: e.span() },
                Token::Float(val) = e => Expression { kind: ExprKind::Literal(LiteralValue::Float(val)), span: e.span() },
                Token::String(s) = e => Expression { kind: ExprKind::Literal(LiteralValue::String(s)), span: e.span() },
                Token::Boolean(b) = e => Expression { kind: ExprKind::Literal(LiteralValue::Bool(b)), span: e.span() },
            },
            ident.clone().map(|ident| Expression { span: ident.span.clone(), kind: ExprKind::Variable(ident) }),
            expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)),
        ));
        
        // --- 运算符优先级金字塔 (所有逻辑保持不变) ---
        let call = atom.clone().then(
            expr.clone().separated_by(just(Token::Comma)).allow_trailing().collect()
                .delimited_by(just(Token::LParen), just(Token::RParen)).or_not()
        ).map_with(|(callee, opt_call), e| {
            if let Some(args) = opt_call {
                if let ExprKind::Variable(name) = callee.kind {
                    Expression { kind: ExprKind::FunctionCall { name, args }, span: e.span() }
                } else { callee }
            } else { callee }
        });

        let op = |c| just(c);
        let unary_op = op(Token::Minus).to(UnaryOp::Negate)
            .or(op(Token::Not).to(UnaryOp::Not))
            .or(op(Token::Ampersand).to(UnaryOp::AddressOf)) 
            .or(op(Token::Star).to(UnaryOp::Dereference));
        
        let unary = unary_op.repeated().foldr(call, |op, right| {
            let span = right.span.clone();
            Expression { kind: ExprKind::UnaryOp { op, right: Box::new(right) }, span }
        });

        let product_op = op(Token::Star).to(BinaryOp::Multiply).or(op(Token::Slash).to(BinaryOp::Divide)).or(op(Token::Percent).to(BinaryOp::Modulo));
        let product = unary.clone().foldl(product_op.then(unary).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });
        
        let sum_op = op(Token::Plus).to(BinaryOp::Add).or(op(Token::Minus).to(BinaryOp::Subtract));
        let sum = product.clone().foldl(sum_op.then(product).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });
        
        let relational_op = op(Token::Lt).to(BinaryOp::Lt).or(op(Token::Lte).to(BinaryOp::Lte))
            .or(op(Token::Gt).to(BinaryOp::Gt)).or(op(Token::Gte).to(BinaryOp::Gte));
        let relation = sum.clone().foldl(relational_op.then(sum).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });

        let equality_op = op(Token::Eq).to(BinaryOp::Eq).or(op(Token::NotEq).to(BinaryOp::NotEq));
        let equality = relation.clone().foldl(equality_op.then(relation).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });
        
        let logical_and = equality.clone().foldl(op(Token::And).to(BinaryOp::And).then(equality).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });
        
        let logical_or = logical_and.clone().foldl(op(Token::Or).to(BinaryOp::Or).then(logical_and).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });

        logical_or.clone().then(op(Token::Assign).to(()).then(expr.clone()).or_not())
            .map_with(|(left, right_opt), e| {
                if let Some((_, right)) = right_opt {
                    Expression { kind: ExprKind::Assignment { left: Box::new(left), right: Box::new(right) }, span: e.span() }
                } else { left }
            })
            .labelled("expression")
    });
    
    // --- 语句和代码块解析器 (所有逻辑保持不变) ---
    let var_decl = just(Token::Const).or_not()
        .then(ident.clone()) // 1. 先匹配名字
        .then_ignore(just(Token::Colon)) // 2. 然后匹配并忽略冒号
        .then(type_.clone()) // 3. 然后匹配类型
        .then(just(Token::Assign).ignore_then(expr.clone()).or_not())
        .then_ignore(just(Token::Semicolon))
        // 4. 更新 map_with 的解构，以匹配新的元组顺序
        .map_with(|(((is_const, name), var_type), init), e| VarDecl {
            is_const: is_const.is_some(),
            var_type, // <-- var_type 和 name 的位置换了
            name,     // <--
            init,
            span: e.span(),
        });
    
    let stmt = choice((
        block.clone().map(Statement::Block),
        just(Token::Return)
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(Token::Semicolon))
            .map_with(|value, e| Statement::Return { value, span: e.span() }),
        just(Token::If)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .map_with(|((condition, then_branch), else_branch), e| Statement::If {
                condition, then_branch: Box::new(then_branch), else_branch: else_branch.map(Box::new), span: e.span(),
            }),
        just(Token::While)
            .ignore_then(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(block.clone())
            .map_with(|(condition, body), e| Statement::While {
                condition, body: Box::new(body), span: e.span(),
            }),
        just(Token::Break).then_ignore(just(Token::Semicolon)).map_with(|_, e| Statement::Break(e.span())),
        just(Token::Continue).then_ignore(just(Token::Semicolon)).map_with(|_, e| Statement::Continue(e.span())),
        var_decl.clone().map(Statement::VarDecl),
        expr.clone().then_ignore(just(Token::Semicolon)).map(Statement::Expr),
    ))
    .labelled("statement");
    
    block.define(
        stmt
            .repeated()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|stmts, e| Block { stmts, span: e.span() })
            .labelled("block")
    );

    // --- 顶层解析器 (所有逻辑保持不变) ---
    let func_def = ident.clone() // 1. 先匹配函数名
        .then( // 2. 然后匹配参数列表
            // [FIX] 定义一个能解析 "name: type" 的新规则
            ident.clone()
                .then_ignore(just(Token::Colon))
                .then(type_.clone())
                // [CRITICAL] 将解析结果 (Ident, Type) 转换回 AST 期望的 (Type, Ident)
                .map(|(ident, type_)| (type_, ident)) 
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then( // 3. 然后匹配箭头和返回类型
            just(Token::Arrow)
                .ignore_then(type_.clone())
        )
        .then(block.clone()) // 4. 最后匹配函数体
        .map_with(|(((name, params), return_type), body), e| {
            GlobalItem::Function(FunctionDef {
                return_type,
                name,
                params, // <-- 这里接收到的 params 现在是正确的 Vec<(Type, Ident)>
                body,
                span: e.span(),
            })
        })
        .labelled("function definition");
    
    let item = choice((
        func_def,
        var_decl.map(GlobalItem::VarDecl),
    )).boxed();

    item.repeated()
        .collect()
        .map_with(|items, e| Program { items, span: e.span() })
        .then_ignore(end())
}


/// 辅助函数：类型解析器 (保持不变)
fn type_parser<'a, I>() -> impl Parser<'a, I, Type, ParseError<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    // [MODIFIED] `select!` 宏现在可以识别所有新的类型 Token
    let base_type = select! {
        // 传统类型
        Token::Int => Type::I32, // 将旧的 int 直接映射到新的 I32
        Token::Char => Type::Char,
        Token::Bool => Type::Bool,
        Token::Void => Type::Void,

        // 整数类型
        Token::I8 => Type::I8,
        Token::I16 => Type::I16,
        Token::I32 => Type::I32,
        Token::I64 => Type::I64,
        Token::U8 => Type::U8,
        Token::U16 => Type::U16,
        Token::U32 => Type::U32,
        Token::U64 => Type::U64,

        // 浮点数类型
        Token::F32 => Type::F32,
        Token::F64 => Type::F64,
    };

    // 指针解析逻辑保持不变
    base_type
        .then(
            just(Token::Star)
            .repeated()
            .collect::<Vec<_>>()
        )
        .map(|(mut base, pointers)| {
            for _ in pointers {
                base = Type::Ptr(Box::new(base));
            }
            base
        })
        .labelled("type")
}
