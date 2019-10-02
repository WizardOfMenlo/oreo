use crate::lexical::EnrichedToken;
use crate::parsing_utils::*;
use crate::syntax::*;
use crate::tokens::*;
use std::iter::Peekable;

pub(crate) type ExpToken = Token<'static>;
pub(crate) type TokenList = &'static [ExpToken];

pub(crate) mod consts {
    use super::{ExpToken, TokenList};
    use crate::tokens::*;

    pub(crate) const ID: ExpToken = Token::Identifier("");
    pub(crate) const INT: Token<'static> = Token::Literal(Literal::Integer(0));
    pub(crate) const STRING: Token<'static> = Token::Literal(Literal::String(""));
    pub(crate) const BOOL: ExpToken = Token::Literal(Literal::Boolean(false));

    pub(crate) const END: ExpToken = Token::Keyword(Keyword::End);
    pub(crate) const VAR: ExpToken = Token::Keyword(Keyword::Var);

    pub(crate) const SEMICOLON: ExpToken = Token::Punctuation(Punctuation::Semicolon);
    pub(crate) const B_OPEN: ExpToken = Token::Punctuation(Punctuation::BracketOpen);
    pub(crate) const B_CLOSE: ExpToken = Token::Punctuation(Punctuation::BracketClose);

    pub(crate) const RELATIONAL_OPS: TokenList = &[
        Token::Operator(Operator::LesserOrEquals),
        Token::Operator(Operator::GreaterOrEquals),
        Token::Operator(Operator::GreaterThan),
        Token::Operator(Operator::LesserThan),
        Token::Operator(Operator::Equals),
    ];

    pub(crate) const POSSIBLE_STATEMENT_STARTS: TokenList = &[
        Token::Keyword(Keyword::Var),
        Token::Keyword(Keyword::If),
        Token::Keyword(Keyword::Print),
        Token::Keyword(Keyword::Println),
        Token::Keyword(Keyword::Get),
        Token::Keyword(Keyword::While),
        Token::Keyword(Keyword::Procedure),
        Token::Keyword(Keyword::Return),
        ID,
    ];

    pub(crate) const POSSIBLE_EXPR_PRIME_STARTS: TokenList = &[
        Token::Operator(Operator::Plus),
        Token::Operator(Operator::Minus),
        Token::Operator(Operator::Divide),
        Token::Operator(Operator::Times),
    ];

    pub(crate) const POSSIBLE_BOOL_PRIME_STARTS: TokenList = &[
        Token::Operator(Operator::Plus),
        Token::Operator(Operator::Minus),
        Token::Operator(Operator::Divide),
        Token::Operator(Operator::Times),
        Token::Operator(Operator::And),
        Token::Operator(Operator::Or),
        Token::Operator(Operator::LesserOrEquals),
        Token::Operator(Operator::GreaterOrEquals),
        Token::Operator(Operator::GreaterThan),
        Token::Operator(Operator::LesserThan),
        Token::Operator(Operator::Equals),
    ];
}

#[derive(Debug)]
pub enum SyntaxError<'a> {
    ExpectedOneOfButFoundEOF(TokenList),
    ExpectedButFoundEOF(ExpToken),
    ExpectedFound(ExpToken, EnrichedToken<'a>),
    ExpectedOneOfFound(TokenList, EnrichedToken<'a>),
    LogicalError,
}

pub fn parse<'a>(
    it: impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Program<'a>, SyntaxError<'a>> {
    // Note we skip comments completely
    let mut it = it.filter(|s| !s.token().is_comment()).peekable();
    program(&mut it)
}

fn program<'a, T>(it: &mut Peekable<T>) -> Result<Program<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // Program -> program id; Compound
    advance_expecting(it, Token::Keyword(Keyword::Program))?;
    let id = advance_expecting_identifier(it)?;
    let compound = compound(it)?;

    Ok(Program { id, compound })
}

fn compound<'a, T>(it: &mut Peekable<T>) -> Result<Compound<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    use consts::END;

    // Compound -> Statement+
    advance_expecting(it, Token::Keyword(Keyword::Begin))?;
    let mut statements = Vec::new();
    loop {
        if let Some(tok) = it.peek() {
            let token = tok.token();
            if token.same_kind(&END) {
                break;
            }

            statements.push(statement(it)?);
        } else {
            return Err(SyntaxError::ExpectedButFoundEOF(END));
        }
    }

    advance_expecting(it, END)?;

    Ok(Compound { statements })
}

fn statement<'a, T>(it: &mut Peekable<T>) -> Result<Statement<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // Statement -> Decl | If | Print | While | Procedure | Return | Assign

    let matched = advance_expecting_one_of(it, consts::POSSIBLE_STATEMENT_STARTS)?;
    let token = matched.take_token();
    Ok(match token {
        Token::Keyword(Keyword::Var) => Statement::Decl(decl(it)?),
        Token::Keyword(Keyword::If) => Statement::If(p_if(it)?),
        Token::Keyword(Keyword::Print) => Statement::Print(print(it, token)?),
        Token::Keyword(Keyword::Println) => Statement::Print(print(it, token)?),
        Token::Keyword(Keyword::Get) => Statement::Print(print(it, token)?),
        Token::Keyword(Keyword::While) => Statement::While(p_while(it)?),
        Token::Keyword(Keyword::Procedure) => Statement::FunctionDecl(function(it)?),
        Token::Keyword(Keyword::Return) => Statement::Return(p_return(it)?),
        Token::Identifier(s) => Statement::Assign(assign(it, Identifier(s))?),
        _ => return Err(SyntaxError::LogicalError),
    })
}

fn decl<'a, T>(it: &mut Peekable<T>) -> Result<Decl<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // Decl -> var id := Expr; | var id;
    let id = advance_expecting_identifier(it)?;
    let next = advance_expecting_one_of(
        it,
        &[consts::SEMICOLON, Token::Operator(Operator::Assignement)],
    )?;

    Ok(match next.token() {
        Token::Punctuation(Punctuation::Semicolon) => Decl { id, expr: None },
        Token::Operator(Operator::Assignement) => {
            let expr = Some(expr(it)?);
            advance_expecting(it, consts::SEMICOLON)?;
            Decl { id, expr }
        }
        _ => return Err(SyntaxError::LogicalError),
    })
}

fn p_if<'a, T>(it: &mut Peekable<T>) -> Result<If<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // If -> if (Bool) then Compound; | if (Bool) then Compound else Compound;
    advance_expecting(it, consts::B_OPEN)?;
    let condition = p_bool(it)?;
    advance_expecting(it, consts::B_CLOSE)?;
    advance_expecting(it, Token::Keyword(Keyword::Then))?;
    let if_branch = compound(it)?;
    let next = advance_expecting_one_of(it, &[Token::Keyword(Keyword::Else), consts::SEMICOLON])?;

    let else_branch = match next.token() {
        Token::Keyword(Keyword::Else) => {
            let tmp = compound(it)?;
            advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;
            Some(tmp)
        }
        &consts::SEMICOLON => None,
        _ => return Err(SyntaxError::LogicalError),
    };

    Ok(If {
        condition,
        if_branch,
        else_branch,
    })
}

fn print<'a, T>(it: &mut Peekable<T>, token: Token<'a>) -> Result<Print<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // Print -> print Expr; | println Expr; | get Expr;
    let print_stat = match token {
        Token::Keyword(Keyword::Print) => Print::Print(expr(it)?),
        Token::Keyword(Keyword::Println) => Print::Println(expr(it)?),
        Token::Keyword(Keyword::Get) => Print::Get(advance_expecting_identifier(it)?),
        // Should only be called with normal stuff
        _ => return Err(SyntaxError::LogicalError),
    };

    advance_expecting(it, consts::SEMICOLON)?;

    Ok(print_stat)
}

fn p_while<'a, T>(it: &mut Peekable<T>) -> Result<While<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // While -> while (Bool) Compound;
    advance_expecting(it, consts::B_OPEN)?;
    let condition = p_bool(it)?;
    advance_expecting(it, consts::B_CLOSE)?;
    let compound = compound(it)?;
    advance_expecting(it, consts::SEMICOLON)?;

    Ok(While {
        condition,
        compound,
    })
}

fn function<'a, T>(it: &mut Peekable<T>) -> Result<FunctionDecl<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // Procedure -> procedure id((var i,)*) begin Compound end
    let id = advance_expecting_identifier(it)?;
    advance_expecting(it, consts::B_OPEN)?;

    let mut args = Vec::new();
    loop {
        if let Some(tok) = it.peek() {
            if tok.token().same_kind(&consts::B_CLOSE) {
                break;
            }

            advance_expecting(it, consts::VAR)?;
            let arg_name = advance_expecting_identifier(it)?;
            args.push(arg_name);

            if it
                .peek()
                .map(|t| t.token().same_kind(&consts::B_CLOSE))
                .unwrap_or(false)
            {
                break;
            }

            advance_expecting(it, Token::Punctuation(Punctuation::Comma))?;
        } else {
            return Err(SyntaxError::ExpectedOneOfButFoundEOF(&[
                consts::B_CLOSE,
                consts::VAR,
            ]));
        }
    }

    advance_expecting(it, consts::B_CLOSE)?;

    let inner = compound(it)?;

    Ok(FunctionDecl { id, args, inner })
}

fn p_return<'a, T>(it: &mut Peekable<T>) -> Result<Return<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // Return -> return Expr;
    let expr = expr(it)?;
    advance_expecting(it, consts::SEMICOLON)?;
    Ok(Return { expr })
}

fn assign<'a, T>(it: &mut Peekable<T>, id: Identifier<'a>) -> Result<Assign<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    // Assign -> id := Expr;
    advance_expecting(it, Token::Operator(Operator::Assignement))?;
    let expr = expr(it)?;
    advance_expecting(it, consts::SEMICOLON)?;

    Ok(Assign { id, expr })
}

// Utility for getting the expr tail
fn get_expr_tail<'a, T>(it: &mut Peekable<T>) -> Result<Option<ExprPrime<'a>>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    match it
        .peek()
        .map(|t| consts::POSSIBLE_EXPR_PRIME_STARTS.contains(t.token()))
    {
        Some(true) => Ok(Some(expr_prime(it)?)),
        _ => Ok(None),
    }
}

fn expr<'a, T>(it: &mut Peekable<T>) -> Result<Expr<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let head = expr_head(it)?;
    let tail = get_expr_tail(it)?.map(Box::new);

    Ok(Expr { head, tail })
}

fn expr_head<'a, T>(it: &mut Peekable<T>) -> Result<ExprHead<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let next = it.peek();
    match next.map(|t| t.token()) {
        Some(&consts::B_OPEN) => {
            advance_expecting(it, consts::B_OPEN)?;
            let expr = expr(it)?;
            advance_expecting(it, consts::B_CLOSE)?;
            Ok(ExprHead::BracketedExpr(Box::new(expr)))
        }
        Some(Token::Literal(Literal::Integer(_)))
        | Some(Token::Literal(Literal::String(_)))
        | Some(Token::Identifier(_)) => Ok(ExprHead::Unit(unit(it)?)),

        Some(_) => Ok(ExprHead::Boolean(Box::new(p_bool(it)?))),

        // TODO: This should be a EOF error
        _ => Err(SyntaxError::LogicalError),
    }
}

fn unit<'a, T>(it: &mut Peekable<T>) -> Result<Unit<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let next = advance_expecting_one_of(it, &[consts::INT, consts::STRING, consts::ID])?;
    Ok(match next.take_token() {
        Token::Literal(Literal::Integer(i)) => Unit::Int(i),
        Token::Literal(Literal::String(s)) => Unit::String(s),
        Token::Identifier(s) => {
            let next = it.peek().map(|t| t.token());
            match next {
                Some(&consts::B_OPEN) => Unit::FunctionCall(function_call(it, Identifier(s))?),
                _ => Unit::Identifier(Identifier(s)),
            }
        }
        _ => return Err(SyntaxError::LogicalError),
    })
}

fn function_call<'a, T>(
    it: &mut Peekable<T>,
    id: Identifier<'a>,
) -> Result<FunctionCall<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    advance_expecting(it, consts::B_OPEN)?;

    let mut args = Vec::new();
    loop {
        if let Some(tok) = it.peek() {
            if tok.token().same_kind(&consts::B_CLOSE) {
                break;
            }

            let expr = expr(it)?;
            args.push(expr);

            if it
                .peek()
                .map(|t| t.token().same_kind(&consts::B_CLOSE))
                .unwrap_or(false)
            {
                break;
            }

            advance_expecting(it, Token::Punctuation(Punctuation::Comma))?;
        } else {
            return Err(SyntaxError::ExpectedButFoundEOF(consts::B_CLOSE));
        }
    }

    advance_expecting(it, consts::B_CLOSE)?;

    Ok(FunctionCall { id, args })
}

fn expr_prime<'a, T>(it: &mut Peekable<T>) -> Result<ExprPrime<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let operator = advance_expecting_one_of(it, consts::POSSIBLE_EXPR_PRIME_STARTS)?;

    let operand = expr(it)?;
    let tail = get_expr_tail(it)?.map(Box::new);

    Ok(ExprPrime {
        operator: match operator.token() {
            Token::Operator(Operator::Plus) => BinaryExprOp::Plus,
            Token::Operator(Operator::Minus) => BinaryExprOp::Minus,
            Token::Operator(Operator::Times) => BinaryExprOp::Times,
            Token::Operator(Operator::Divide) => BinaryExprOp::Divide,
            _ => return Err(SyntaxError::LogicalError),
        },
        operand,
        tail,
    })
}

fn get_bool_tail<'a, T>(it: &mut Peekable<T>) -> Result<Option<Box<BoolTail<'a>>>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    Ok(
        match it
            .peek()
            .map(|t| t.token())
            .map(|t| consts::POSSIBLE_BOOL_PRIME_STARTS.contains(t))
        {
            Some(true) => Some(Box::new(boolean_tail(it)?)),
            _ => None,
        },
    )
}

fn p_bool<'a, T>(it: &mut Peekable<T>) -> Result<Bool<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let head = Box::new(boolean_head(it)?);

    let tail = get_bool_tail(it)?;

    Ok(Bool { head, tail })
}

fn boolean_head<'a, T>(it: &mut Peekable<T>) -> Result<BooleanHead<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let next = it.peek();
    Ok(match next.cloned().map(|t| t.take_token()) {
        Some(Token::Operator(Operator::Not)) => {
            advance_expecting(it, Token::Operator(Operator::Not))?;
            BooleanHead::Not(p_bool(it)?)
        }
        Some(Token::Literal(Literal::Boolean(b))) => {
            advance_expecting(it, consts::BOOL)?;
            BooleanHead::BooleanLiteral(b)
        }
        _ => BooleanHead::RelationalOperation(relational_operation(it)?),
    })
}

fn relational_operation<'a, T>(
    it: &mut Peekable<T>,
) -> Result<RelationalOperation<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let head = boolean_unit(it)?;
    let tail = relational_operation_tail(it)?;

    Ok(RelationalOperation { head, tail })
}

fn boolean_unit<'a, T>(it: &mut Peekable<T>) -> Result<BooleanUnit<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let next = it.peek();
    Ok(match next.map(|t| t.token()) {
        Some(&consts::B_OPEN) => {
            advance_expecting(it, consts::B_OPEN)?;
            let expr = expr(it)?;
            advance_expecting(it, consts::B_CLOSE)?;
            BooleanUnit::BracketedExpr(expr)
        }
        _ => BooleanUnit::Unit(unit(it)?),
    })
}

fn relational_operation_tail<'a, T>(
    it: &mut Peekable<T>,
) -> Result<RelationalOperationTail<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let opt_lhs_tail = get_expr_tail(it)?;

    let rel = advance_expecting_one_of(it, consts::RELATIONAL_OPS)?;
    let operator = match rel.token() {
        Token::Operator(Operator::GreaterThan) => RelationalOperator::GreaterThan,
        Token::Operator(Operator::LesserThan) => RelationalOperator::LesserThan,
        Token::Operator(Operator::GreaterOrEquals) => RelationalOperator::GreaterOrEquals,
        Token::Operator(Operator::LesserOrEquals) => RelationalOperator::LesserOrEquals,
        Token::Operator(Operator::Equals) => RelationalOperator::Equals,
        _ => return Err(SyntaxError::LogicalError),
    };

    let rhs = expr(it)?;

    Ok(RelationalOperationTail {
        opt_lhs_tail,
        operator,
        rhs,
    })
}

fn boolean_tail<'a, T>(it: &mut Peekable<T>) -> Result<BoolTail<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let head = bool_tail_head_expr(it)?;
    let tail = get_bool_tail(it)?;

    Ok(BoolTail { head, tail })
}

fn bool_tail_head_expr<'a, T>(it: &mut Peekable<T>) -> Result<BoolTailHeadExpr<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let next = it.peek();
    Ok(match next.map(|t| t.token()) {
        Some(Token::Operator(Operator::And)) | Some(Token::Operator(Operator::Or)) => {
            BoolTailHeadExpr::BooleanOperation(boolean_operation(it)?)
        }
        _ => BoolTailHeadExpr::RelationalOperationTail(relational_operation_tail(it)?),
    })
}

fn boolean_operation<'a, T>(it: &mut Peekable<T>) -> Result<BooleanOperation<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let next = advance_expecting_one_of(
        it,
        &[
            Token::Operator(Operator::Or),
            Token::Operator(Operator::And),
        ],
    )?;
    let op = match next.token() {
        Token::Operator(Operator::Or) => BooleanOperator::Or,
        Token::Operator(Operator::And) => BooleanOperator::And,
        _ => return Err(SyntaxError::LogicalError),
    };
    let rhs = p_bool(it)?;

    Ok(BooleanOperation { op, rhs })
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    fn make_tokens_from_str(s: &str) -> impl Iterator<Item = EnrichedToken> {
        use crate::lexical::lexicalize;
        use crate::scanner::scan;

        lexicalize(scan(s))
    }

    #[test]
    fn parse_empty() {
        let parsed = parse(make_tokens_from_str(""));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_invalid() {
        let parsed = parse(make_tokens_from_str("(x <= > >= < y 99.88l8 )"));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_minimal_no_bool_no_expr() {
        let input = "program fib begin var n; end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_minimal_assignments() {
        let input = "program fib begin var n := \"Hello\"; var m := 2; var s := (2); end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_print_simple() {
        let input = "program fib begin print \"Hello\\n\"; println 2; get x; end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_no_args() {
        let input = "program fib begin procedure main() begin return 1; end end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_single_arg() {
        let input = "program fib begin procedure id(var x) begin return x; end end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_mult_arg() {
        let input = "program fib begin procedure sum(var x, var y) begin return y; end end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_no_arg_call() {
        let input = "program fib begin var x := f(); end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_single_arg_call() {
        let input = "program fib begin var x := id(x*y); end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_multiple_arg_call() {
        let input = "program fib begin var x := sum(x+y, y+x); end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_condition() {
        let input = r#"
        program x
        begin
            while (c < n)
            begin
                c := c + 1;
            end;
        end
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_condition_nested() {
        let input = r#"
        program x
        begin
            while (c < (n < 1))
            begin
                c := c + 1;
            end;
        end
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_bool_expr_edge() {
        let input = r#"
        program x
        begin
            var c := 0 < (c < 1);
        end
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_full() {
        let input = r#"
program fib
begin
    var n;
    var first := 0;
    var second :=1;
    var next;
    var c :=0 ;
    print "enter the number of terms";
    get n;
    while ( c < n)
        begin
        if ( c <= 1)
        then begin next := c; end
        else begin
            next := first + second;
            second := next;
        end;
    print next;
    c := c + 1;
    end;
end"#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_1() {
        let input = r#"
        {- this should parse without errors -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0 ; 
	
    print "enter the number of terms";
    
    get n;  
    
    while ( first < (second > 32))
    begin

       if ( true ) then begin n := 5;  end
       			   else begin n := 46; end ;
                        
                                  
    c := c + 1;	    
	end;
end    
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_2() {
        let input = r#"
        {- missing a semi colon -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0  
	
    print "enter the number of terms";
    
    get n;  
    
    while ( first < (second > 32))
    begin

       if ( true ) then begin n := 5;  end
       			   else begin n := 46; end;
                        
                                  
    c := c + 1;	    
	end;
end     
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_3() {
        let input = r#"
        {- 2 semi colons -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;;
	
    print "enter the number of terms";
    
    get n;  
    
    while ( first < (second > 32))
    begin

       if ( true ) then begin n := 5;  end
       			   else begin n := 46; end;
                        
                                  
    c := c + 1;	    
	end;
end     
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_4() {
        let input = r#"
        {- missing bracket -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;
	
    print "enter the number of terms";
    
    get n;  
    
    while ( first < (second > 32)
    begin

       if ( true ) then begin n := 5;  end
       			   else begin n := 46; end
                        ;
                                  
    c := c + 1;	    
	end;
end     
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_5() {
        let input = r#"
        {- missing end -}
program test5
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;
	
    print "enter the number of terms";
    
    get n;  
    
    while ( first < (second > 32)  )
    begin

       if ( true ) then begin n := 5;  end
       			   else begin n := 46; end; 
                        
                                  
    c := c + 1;	    
	
end    
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_6() {
        let input = r#"
        {- ambiguous expression in if -}
program test6
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;
	
    print "enter the number of terms";
    
    get n;  
    
    while ( first < (second > 32)  )
    begin

       if ( not false or true ) then begin n := 5;  end
       			        else begin n := 46; end;
                        
                                  
    c := c + 1;	    
	end;
end    
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_7() {
        let input = r#"
        {- string not closed -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;
	
    print "enter the number of terms;
    
    get n;  
    
    while ( first < (second > 32)  )
    begin

       if ( 1 > 2 ) then begin n := 5;  end
       			   else begin n := 46; end;
                        
                                  
    c := c + 1;	    
	end;
end       
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_8() {
        let input = r#"
        {- will parse OK -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;
	
    print "enter the number of terms";
                 {- comment in body -}
    get n;  
    
    while ( first < (second > 32)  )
    begin

       if ( 42 < 100 ) then begin n := 5;  end
       			   else begin n := 46; end;
                        {- comment
                        over
                        multiple 
                        lines -}
                                  
    c := c + 1;	    
	end;
end    
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_9() {
        let input = r#"{- non bool in if -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;
	
    print "enter the number of terms";
                 {- comment in body -}
    get n;  
    
    while ( first < (second > 32)  )
    begin

       if ( 42 ) then begin n := 5;  end
       			   else begin n := 46; end;
                        {- comment
                        over
                        multiple 
                        lines -}
                                  
    c := c + 1;	    
	end;
end    "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_oreo_10() {
        let input = r#"
        {- invalid ID -}
program test10
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var 99 :=0;
	
    print "enter the number of terms";
                 {- comment in body -}
    get n;  
    
    while ( first < (second > 32)  )
    begin

       if ( not false) then begin n := 5;  end
       			   else begin n := 46; end;
                        {- comment
                        over
                        multiple 
                        lines -}
                                  
    c := c + 1;	    
	end;
end       
        "#;
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }
}
