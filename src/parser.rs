use crate::lexical::EnrichedToken;
use crate::parsing_utils::*;
use crate::syntax::*;
use crate::tokens::*;
use serde::Serialize;

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

    pub(crate) const ADDITIVE_OPS: TokenList = &[
        Token::Operator(Operator::Plus),
        Token::Operator(Operator::Minus),
    ];

    pub(crate) const MULTIPLICATIVE_OPS: TokenList = &[
        Token::Operator(Operator::Times),
        Token::Operator(Operator::Divide),
    ];

    pub(crate) const RELATIONAL_OPS: TokenList = &[
        Token::Operator(Operator::LesserOrEquals),
        Token::Operator(Operator::GreaterOrEquals),
        Token::Operator(Operator::GreaterThan),
        Token::Operator(Operator::LesserThan),
        Token::Operator(Operator::Equals),
    ];

    pub(crate) const BOOL_OPS: TokenList = &[
        Token::Operator(Operator::And),
        Token::Operator(Operator::Or),
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

    pub(crate) const PRINT_STARTS: TokenList = &[
        Token::Keyword(Keyword::Print),
        Token::Keyword(Keyword::Println),
        Token::Keyword(Keyword::Get),
    ];

    pub(crate) const POSSIBLE_UNIT_STARTS: TokenList = &[ID, B_OPEN, STRING, INT, BOOL];

    pub(crate) const POSSIBLE_EXPR_PRIME_STARTS: TokenList = BOOL_OPS;

    pub(crate) const POSSIBLE_TERM_PRIME_STARTS: TokenList = RELATIONAL_OPS;

    pub(crate) const POSSIBLE_FACTOR_PRIME_STARTS: TokenList = ADDITIVE_OPS;

    pub(crate) const POSSIBLE_PRODUCT_PRIME_STARTS: TokenList = MULTIPLICATIVE_OPS;

    pub(crate) const POSSIBLE_ATOM_START: TokenList = &[
        Token::Operator(Operator::Not),
        ID,
        B_OPEN,
        STRING,
        BOOL,
        INT,
    ];
}

#[derive(Debug, Clone, Serialize)]
pub enum SyntaxError<'a> {
    ExpectedOneOfButFoundEOF(TokenList),
    ExpectedButFoundEOF(ExpToken),
    ExpectedFound(ExpToken, EnrichedToken<'a>),
    ExpectedOneOfFound(TokenList, EnrichedToken<'a>),
    LogicalError,
}

pub type ParsingResult<'a, T> = Result<T, SyntaxError<'a>>;

pub fn parse<'a>(it: impl Iterator<Item = EnrichedToken<'a>>) -> ParsingResult<'a, Program<'a>> {
    // Note we skip comments completely
    let mut it = CollectorStream::new(it.filter(|s| !s.token().is_comment()).peekable());
    program(&mut it)
}

fn program<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Program<'a>> {
    // Program -> program id; Compound
    advance_expecting(it, Token::Keyword(Keyword::Program))?;
    let id = advance_expecting_identifier(it)?;
    let compound = compound(it)?;

    Ok(Program { id, compound })
}

fn compound<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Compound<'a>> {
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

fn statement<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Statement<'a>> {
    // Statement -> Decl | If | Print | While | Procedure | Return | Assign

    let matched = peek_expecting_one_of(it, consts::POSSIBLE_STATEMENT_STARTS)?;
    let token = matched.token();
    Ok(match token {
        Token::Keyword(Keyword::Var) => Statement::Decl(decl(it)?),
        Token::Keyword(Keyword::If) => Statement::If(p_if(it)?),

        Token::Keyword(Keyword::Print)
        | Token::Keyword(Keyword::Println)
        | Token::Keyword(Keyword::Get) => Statement::Print(print(it)?),

        Token::Keyword(Keyword::While) => Statement::While(p_while(it)?),
        Token::Keyword(Keyword::Procedure) => Statement::FunctionDecl(function(it)?),
        Token::Keyword(Keyword::Return) => Statement::Return(p_return(it)?),
        Token::Identifier(_) => Statement::Assign(assign(it)?),
        _ => return Err(SyntaxError::LogicalError),
    })
}

fn decl<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Decl<'a>> {
    // Decl -> var id := Expr; | var id;
    advance_expecting(it, Token::Keyword(Keyword::Var))?;
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

fn p_if<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, If<'a>> {
    // If -> if (Bool) then Compound; | if (Bool) then Compound else Compound;
    advance_expecting(it, Token::Keyword(Keyword::If))?;
    advance_expecting(it, consts::B_OPEN)?;
    let condition = expr(it)?;
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

fn print<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Print<'a>> {
    // Print -> print Expr; | println Expr; | get Expr;
    let next = advance_expecting_one_of(it, consts::PRINT_STARTS)?;

    let print_stat = match next.token() {
        Token::Keyword(Keyword::Print) => Print::Print(expr(it)?),
        Token::Keyword(Keyword::Println) => Print::Println(expr(it)?),
        Token::Keyword(Keyword::Get) => Print::Get(advance_expecting_identifier(it)?),
        // Should only be called with normal stuff
        _ => return Err(SyntaxError::LogicalError),
    };

    advance_expecting(it, consts::SEMICOLON)?;

    Ok(print_stat)
}

fn p_while<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, While<'a>> {
    // While -> while (Bool) Compound;
    advance_expecting(it, Token::Keyword(Keyword::While))?;
    advance_expecting(it, consts::B_OPEN)?;
    let condition = expr(it)?;
    advance_expecting(it, consts::B_CLOSE)?;
    let compound = compound(it)?;
    advance_expecting(it, consts::SEMICOLON)?;

    Ok(While {
        condition,
        compound,
    })
}

fn function<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, FunctionDecl<'a>> {
    // Procedure -> procedure id((var i,)*) begin Compound end
    advance_expecting(it, Token::Keyword(Keyword::Procedure))?;
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

fn p_return<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Return<'a>> {
    // Return -> return Expr;
    advance_expecting(it, Token::Keyword(Keyword::Return))?;
    let expr = expr(it)?;
    advance_expecting(it, consts::SEMICOLON)?;
    Ok(Return { expr })
}

fn assign<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Assign<'a>> {
    // Assign -> id := Expr;
    let id = advance_expecting_identifier(it)?;
    advance_expecting(it, Token::Operator(Operator::Assignement))?;
    let expr = expr(it)?;
    advance_expecting(it, consts::SEMICOLON)?;

    Ok(Assign { id, expr })
}

fn expr<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Expr<'a>> {
    let head = term(it)?;
    let tail = get_tail(it, consts::POSSIBLE_EXPR_PRIME_STARTS, expr_prime)?;

    Ok(Expr { head, tail })
}

fn expr_prime<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, ExprPrime<'a>> {
    let operator = advance_expecting_one_of(it, consts::POSSIBLE_EXPR_PRIME_STARTS)?;

    let rhs = term(it)?;
    let tail = get_tail(it, consts::POSSIBLE_EXPR_PRIME_STARTS, expr_prime)?.map(Box::new);

    Ok(ExprPrime {
        op: match operator.token() {
            Token::Operator(Operator::Or) => BooleanOp::Or,
            Token::Operator(Operator::And) => BooleanOp::And,
            _ => return Err(SyntaxError::LogicalError),
        },
        rhs,
        tail,
    })
}

fn term<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Term<'a>> {
    let head = factor(it)?;
    let tail = get_tail(it, consts::POSSIBLE_TERM_PRIME_STARTS, term_prime)?;

    Ok(Term { head, tail })
}

fn term_prime<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, TermPrime<'a>> {
    let operator = advance_expecting_one_of(it, consts::POSSIBLE_TERM_PRIME_STARTS)?;
    let rhs = factor(it)?;
    let tail = get_tail(it, consts::POSSIBLE_TERM_PRIME_STARTS, term_prime)?.map(Box::new);

    Ok(TermPrime {
        op: match operator.token() {
            Token::Operator(Operator::LesserThan) => RelationalOp::LesserThan,
            Token::Operator(Operator::GreaterThan) => RelationalOp::GreaterThan,
            Token::Operator(Operator::LesserOrEquals) => RelationalOp::LesserOrEquals,
            Token::Operator(Operator::GreaterOrEquals) => RelationalOp::GreaterOrEquals,
            Token::Operator(Operator::Equals) => RelationalOp::Equals,
            _ => return Err(SyntaxError::LogicalError),
        },
        rhs,
        tail,
    })
}

fn factor<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Factor<'a>> {
    let head = product(it)?;
    let tail = get_tail(it, consts::POSSIBLE_FACTOR_PRIME_STARTS, factor_prime)?;

    Ok(Factor { head, tail })
}

fn factor_prime<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, FactorPrime<'a>> {
    let operator = advance_expecting_one_of(it, consts::POSSIBLE_FACTOR_PRIME_STARTS)?;
    let rhs = product(it)?;
    let tail = get_tail(it, consts::POSSIBLE_FACTOR_PRIME_STARTS, factor_prime)?.map(Box::new);

    Ok(FactorPrime {
        op: match operator.token() {
            Token::Operator(Operator::Plus) => AdditiveOp::Plus,
            Token::Operator(Operator::Minus) => AdditiveOp::Minus,
            _ => return Err(SyntaxError::LogicalError),
        },
        rhs,
        tail,
    })
}

fn product<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Product<'a>> {
    let head = atom(it)?;
    let tail = get_tail(it, consts::POSSIBLE_PRODUCT_PRIME_STARTS, product_prime)?;

    Ok(Product { head, tail })
}

fn product_prime<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, ProductPrime<'a>> {
    let operator = advance_expecting_one_of(it, consts::POSSIBLE_PRODUCT_PRIME_STARTS)?;
    let rhs = atom(it)?;
    let tail = get_tail(it, consts::POSSIBLE_PRODUCT_PRIME_STARTS, product_prime)?.map(Box::new);

    Ok(ProductPrime {
        op: match operator.token() {
            Token::Operator(Operator::Times) => MultiplicativeOp::Times,
            Token::Operator(Operator::Divide) => MultiplicativeOp::Divide,
            _ => return Err(SyntaxError::LogicalError),
        },
        rhs,
        tail,
    })
}

fn atom<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Atom<'a>> {
    let next = peek_expecting_one_of(it, consts::POSSIBLE_ATOM_START)?;

    Ok(match next.token() {
        Token::Operator(Operator::Not) => {
            advance_expecting(it, Token::Operator(Operator::Not))?;
            Atom::Not(Box::new(atom(it)?))
        }
        _ => Atom::Unit(unit(it)?),
    })
}

fn unit<'a>(it: &mut impl TokenStream<'a>) -> ParsingResult<'a, Unit<'a>> {
    let next = advance_expecting_one_of(it, consts::POSSIBLE_UNIT_STARTS)?;
    Ok(match next.take_token() {
        Token::Literal(Literal::Integer(i)) => Unit::Int(i),
        Token::Literal(Literal::String(s)) => Unit::Str(s),
        Token::Literal(Literal::Boolean(b)) => Unit::Boolean(b),
        Token::Identifier(s) => {
            let next = it.peek().map(|t| t.token());
            match next {
                Some(&consts::B_OPEN) => Unit::FunctionCall(function_call(it, Identifier(s))?),
                _ => Unit::Identifier(Identifier(s)),
            }
        }
        consts::B_OPEN => {
            let expr = Box::new(expr(it)?);
            advance_expecting(it, consts::B_CLOSE)?;
            Unit::BracketedExpr(expr)
        }
        _ => return Err(SyntaxError::LogicalError),
    })
}

fn function_call<'a>(
    it: &mut impl TokenStream<'a>,
    id: Identifier<'a>,
) -> ParsingResult<'a, FunctionCall<'a>> {
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
        let input = "program fib begin procedure sum(var x, var y) begin return x + y; end end";
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
       			   else begin n := 46; end;
                        
                                  
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
