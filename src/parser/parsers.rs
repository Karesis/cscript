// src/parser/main.rs

use crate::lexer::{Token, Span};
use crate::parser::ast::*;
use chumsky::prelude::*;
use chumsky::input::ValueInput;

// 正确的错误类型别名
type ParseError<'a> = extra::Err<Rich<'a, Token, Span>>;

/// 主解析器：解析整个程序。
/// 这个函数现在是我们的“主设置函数”，负责声明和定义所有相互递归的解析器。
pub fn program_parser<'a, I>() -> impl Parser<'a, I, Program, ParseError<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    // =======================================================================
    // 1. 声明 (Declare)
    // =======================================================================
    // 为所有相互递归的解析器创建“占位符”句柄。
    let mut expr = Recursive::declare();
    let mut block = Recursive::declare();

    // =======================================================================
    // 2. 定义 (Define)
    // =======================================================================
    // 在这里，我们将为每个句柄填充实际的解析逻辑。

    // -- 定义非递归的辅助解析器 --
    let ident = select! { Token::Ident(name) = e => Ident { name, span: e.span() } }.labelled("identifier");
    let type_ = type_parser().boxed();

    // -- 定义表达式解析器 (expr) --
    expr.define({
        let atom = choice((
            select! {
                Token::Integer(s) = e => Expression { kind: ExprKind::Literal(LiteralValue::Integer(s.parse().unwrap())), span: e.span() },
                Token::String(s) = e => Expression { kind: ExprKind::Literal(LiteralValue::String(s)), span: e.span() },
                Token::Boolean(b) = e => Expression { kind: ExprKind::Literal(LiteralValue::Bool(b)), span: e.span() },
            },
            ident.clone().map(|ident| Expression { span: ident.span.clone(), kind: ExprKind::Variable(ident) }),
            expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)),
        ));
        
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
        
        // [ADDED] 关系运算符层
        let relational_op = op(Token::Lt).to(BinaryOp::Lt).or(op(Token::Lte).to(BinaryOp::Lte))
            .or(op(Token::Gt).to(BinaryOp::Gt)).or(op(Token::Gte).to(BinaryOp::Gte));
        let relation = sum.clone().foldl(relational_op.then(sum).repeated(), |left, (op, right)| {
             let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });

        // [ADDED] 相等运算符层
        let equality_op = op(Token::Eq).to(BinaryOp::Eq).or(op(Token::NotEq).to(BinaryOp::NotEq));
        let equality = relation.clone().foldl(equality_op.then(relation).repeated(), |left, (op, right)| {
             let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });
        
        // [ADDED] 逻辑与运算符层
        let logical_and = equality.clone().foldl(op(Token::And).to(BinaryOp::And).then(equality).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });
        
        // [ADDED] 逻辑或运算符层
        let logical_or = logical_and.clone().foldl(op(Token::Or).to(BinaryOp::Or).then(logical_and).repeated(), |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Expression { kind: ExprKind::BinaryOp { op, left: Box::new(left), right: Box::new(right) }, span }
        });

        // [UPDATED] 赋值运算符现在是最低优先级
        logical_or.clone().then(op(Token::Assign).to(()).then(expr.clone()).or_not())
            .map_with(|(left, right_opt), e| {
                if let Some((_, right)) = right_opt {
                    Expression { kind: ExprKind::Assignment { left: Box::new(left), right: Box::new(right) }, span: e.span() }
                } else { left }
            })
            .labelled("expression")
    });
    
    // -- 定义语句和代码块解析器 --
    // var_decl 依赖 expr, 所以在 expr 定义后创建
    let var_decl = just(Token::Const).or_not()
        .then(type_.clone())
        .then(ident.clone())
        .then(just(Token::Assign).ignore_then(expr.clone()).or_not())
        .then_ignore(just(Token::Semicolon))
        .map_with(|(((is_const, var_type), name), init), e| VarDecl {
            is_const: is_const.is_some(), var_type, name, init, span: e.span(),
        });
    
    // stmt 依赖 expr 和 block, 所以在它们声明后定义
    let stmt = choice((
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
    
    // block 依赖 stmt, 在 stmt 定义后链接
    block.define(
        stmt
            .repeated()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|stmts, e| Block { stmts, span: e.span() })
            .labelled("block")
    );

    // =======================================================================
    // 3. 组装顶层解析器
    // =======================================================================
    // func_def 依赖 block
    let func_def = type_.clone()
        .then(ident.clone())
        .then(
            type_.clone().then(ident)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(block.clone())
        .map_with(|(((return_type, name), params), body), e| {
            GlobalItem::Function(FunctionDef { return_type, name, params, body, span: e.span() })
        })
        .labelled("function definition");
    
    // 最终的 item 解析器
    let item = choice((
        func_def,
        var_decl.map(GlobalItem::VarDecl),
    )).boxed();

    item.repeated()
        .collect()
        .map_with(|items, e| Program { items, span: e.span() })
        .then_ignore(end())
}


// =======================================================================
// 辅助解析器函数 (非递归部分)
// =======================================================================

fn type_parser<'a, I>() -> impl Parser<'a, I, Type, ParseError<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    let base_type = select! {
        Token::Int => Type::Int,
        Token::Char => Type::Char,
        Token::Bool => Type::Bool,
        Token::Void => Type::Void,
    };
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