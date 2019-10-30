use crate::parsing_utils::{NodeBuilder, ParserStream, PeekMapping, TokenStream};
use crate::range::RangedObject;
use crate::tokens::*;
use crate::untyped::*;

pub(crate) type ExpToken = Token<'static>;
pub(crate) type TokenList = &'static [ExpToken];

pub(crate) mod consts {
    use super::{ExpToken, TokenList};
    use crate::tokens::*;

    pub(crate) const ID: ExpToken = Token::Identifier("");
    pub(crate) const INT: ExpToken = Token::Literal(Literal::Integer(0));
    pub(crate) const STRING: ExpToken = Token::Literal(Literal::String(""));
    pub(crate) const BOOL: ExpToken = Token::Literal(Literal::Boolean(false));

    pub(crate) const SEMICOLON: ExpToken = Token::Punctuation(Punctuation::Semicolon);
    pub(crate) const B_OPEN: ExpToken = Token::Punctuation(Punctuation::BracketOpen);
    pub(crate) const B_CLOSE: ExpToken = Token::Punctuation(Punctuation::BracketClose);

    pub(crate) const IF: ExpToken = Token::Keyword(Keyword::If);
    pub(crate) const PRINT: ExpToken = Token::Keyword(Keyword::Print);
    pub(crate) const PRINTLN: ExpToken = Token::Keyword(Keyword::Println);
    pub(crate) const GET: ExpToken = Token::Keyword(Keyword::Get);
    pub(crate) const WHILE: ExpToken = Token::Keyword(Keyword::While);
    pub(crate) const PROCEDURE: ExpToken = Token::Keyword(Keyword::Procedure);
    pub(crate) const RETURN: ExpToken = Token::Keyword(Keyword::Return);
    pub(crate) const VAR: ExpToken = Token::Keyword(Keyword::Var);
    pub(crate) const END: ExpToken = Token::Keyword(Keyword::End);

    pub(crate) const ASSIGN: ExpToken = Token::Operator(Operator::Assignement);

    pub(crate) const POSSIBLE_DECL_SYMBOLS: TokenList = &[ASSIGN, SEMICOLON];

    pub(crate) const PLUS: ExpToken = Token::Operator(Operator::Plus);
    pub(crate) const MINUS: ExpToken = Token::Operator(Operator::Minus);

    pub(crate) const TIMES: ExpToken = Token::Operator(Operator::Times);
    pub(crate) const DIVIDE: ExpToken = Token::Operator(Operator::Divide);

    pub(crate) const AND: ExpToken = Token::Operator(Operator::And);
    pub(crate) const OR: ExpToken = Token::Operator(Operator::Or);

    pub(crate) const LESS: ExpToken = Token::Operator(Operator::LesserOrEquals);
    pub(crate) const GREAT: ExpToken = Token::Operator(Operator::GreaterOrEquals);
    pub(crate) const LEQ: ExpToken = Token::Operator(Operator::LesserOrEquals);
    pub(crate) const GEQ: ExpToken = Token::Operator(Operator::GreaterOrEquals);
    pub(crate) const EQ: ExpToken = Token::Operator(Operator::Equals);

    pub(crate) const NOT: ExpToken = Token::Operator(Operator::Not);

    pub(crate) const ADDITIVE_OPS: TokenList = &[PLUS, MINUS];

    pub(crate) const MULTIPLICATIVE_OPS: TokenList = &[TIMES, DIVIDE];

    pub(crate) const RELATIONAL_OPS: TokenList = &[LESS, GREAT, GEQ, LEQ, EQ];

    pub(crate) const BOOL_OPS: TokenList = &[AND, OR];

    pub(crate) const POSSIBLE_STATEMENT_STARTS: TokenList =
        &[VAR, IF, PRINT, PRINTLN, GET, WHILE, PROCEDURE, RETURN, ID];

    pub(crate) const PRINT_STARTS: TokenList = &[PRINT, PRINTLN, GET];

    pub(crate) const POSSIBLE_UNIT_STARTS: TokenList = &[ID, B_OPEN, STRING, INT, BOOL];

    pub(crate) const POSSIBLE_EXPR_PRIME_STARTS: TokenList = BOOL_OPS;

    pub(crate) const POSSIBLE_TERM_PRIME_STARTS: TokenList = RELATIONAL_OPS;

    pub(crate) const POSSIBLE_FACTOR_PRIME_STARTS: TokenList = ADDITIVE_OPS;

    pub(crate) const POSSIBLE_PRODUCT_PRIME_STARTS: TokenList = MULTIPLICATIVE_OPS;

    pub(crate) const POSSIBLE_ATOM_START: TokenList = &[NOT, ID, B_OPEN, STRING, BOOL, INT];
}

pub fn parse<'a>(it: impl Iterator<Item = RangedObject<Token<'a>>>) -> Node<'a> {
    // Note we skip comments completely
    let mut it = ParserStream::new(it.filter(|s| !s.inner().is_comment()).peekable());
    program(&mut it)
}

fn program<'a>(it: &mut impl TokenStream<'a>) -> Node<'a> {
    // Program -> program id; Compound
    NodeBuilder::new(NodeType::Program, it)
        .advance_expecting(Token::Keyword(Keyword::Program))
        .children(identifier)
        .children(compound)
        .build()
}

fn identifier<'a>(it: &mut impl TokenStream<'a>) -> Node<'a> {
    NodeBuilder::new(NodeType::Identifier, it)
        .advance_expecting(consts::ID)
        .build()
}

fn compound<'a>(it: &mut impl TokenStream<'a>) -> Node<'a> {
    NodeBuilder::new(NodeType::Compound, it)
        .advance_expecting(Token::Keyword(Keyword::Begin))
        .take_until(consts::END, statement)
        .advance_expecting(consts::END)
        .build()
}

fn statement<'a>(it: &mut impl TokenStream<'a>) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::POSSIBLE_STATEMENT_STARTS,
            PeekMapping::new()
                .add(consts::VAR, decl)
                .add(consts::IF, p_if)
                .add(consts::PRINT, printstat)
                .add(consts::PRINTLN, printstat)
                .add(consts::GET, printstat)
                .add(consts::WHILE, p_while)
                .add(consts::PROCEDURE, func_decl)
                .add(consts::RETURN, p_return)
                .add(consts::ID, assign_or_fun_call),
        )
        .build()
}

fn decl<'a, 'b, T: TokenStream<'a>>(builder: NodeBuilder<'a, 'b, T>) -> NodeBuilder<'a, 'b, T> {
    builder
        .ty(NodeType::Decl)
        .advance_expecting(consts::VAR)
        .children(identifier)
        .peek_and_type(
            consts::POSSIBLE_DECL_SYMBOLS,
            PeekMapping::new()
                .add(consts::ASSIGN, |b: NodeBuilder<'a, 'b, T>| {
                    b.advance_expecting(consts::ASSIGN).children(expr)
                })
                .add(consts::SEMICOLON, |s| s),
        )
        .advance_expecting(consts::SEMICOLON)
}

fn p_if<'a, 'b, T: TokenStream<'a>>(builder: NodeBuilder<'a, 'b, T>) -> NodeBuilder<'a, 'b, T> {
    builder
        .ty(NodeType::If)
        .advance_expecting(consts::IF)
        .advance_expecting(consts::B_OPEN)
        .children(expr)
        .advance_expecting(consts::B_CLOSE)
        .advance_expecting(Token::Keyword(Keyword::Then))
        .children(compound)
        .peek_and_type(
            &[consts::SEMICOLON, Token::Keyword(Keyword::Else)],
            PeekMapping::new()
                .add(consts::SEMICOLON, |s| s)
                .add(Token::Keyword(Keyword::Else), |b| b.children(compound)),
        )
        .advance_expecting(consts::SEMICOLON)
}

fn p_while<'a, 'b, T: TokenStream<'a>>(builder: NodeBuilder<'a, 'b, T>) -> NodeBuilder<'a, 'b, T> {
    builder
        .ty(NodeType::While)
        .advance_expecting(consts::WHILE)
        .advance_expecting(consts::B_OPEN)
        .children(expr)
        .advance_expecting(consts::B_CLOSE)
        .children(compound)
        .advance_expecting(consts::SEMICOLON)
}

fn printstat<'a, 'b, T: TokenStream<'a>>(
    builder: NodeBuilder<'a, 'b, T>,
) -> NodeBuilder<'a, 'b, T> {
    builder.ty(NodeType::PrintStat).children(print)
}

fn print<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::PRINT_STARTS,
            PeekMapping::new()
                .add(consts::PRINT, |b: NodeBuilder<'a, 'b, T>| {
                    b.ty(NodeType::Print)
                        .advance_expecting(consts::PRINT)
                        .children(expr)
                })
                .add(consts::PRINTLN, |b| {
                    b.ty(NodeType::Println)
                        .advance_expecting(consts::PRINTLN)
                        .children(expr)
                })
                .add(consts::GET, |b| {
                    b.ty(NodeType::Get)
                        .advance_expecting(consts::GET)
                        .children(identifier)
                }),
        )
        .build()
}

fn func_decl<'a, 'b, T: TokenStream<'a>>(
    builder: NodeBuilder<'a, 'b, T>,
) -> NodeBuilder<'a, 'b, T> {
    builder
        .ty(NodeType::FunctionDecl)
        .advance_expecting(consts::PROCEDURE)
        .children(identifier)
        .advance_expecting(consts::B_OPEN)
        // TODO: This also allows procedure a(var x,)
        .take_until(consts::B_CLOSE, |it| {
            NodeBuilder::new(NodeType::FunctionDeclArgs, it)
                .advance_expecting(consts::VAR)
                .children(identifier)
                .peek_and_type(
                    &[Token::Punctuation(Punctuation::Comma), consts::B_CLOSE],
                    PeekMapping::new()
                        .add(consts::B_CLOSE, |s| s)
                        .add(Token::Punctuation(Punctuation::Comma), |b| {
                            b.advance_expecting(Token::Punctuation(Punctuation::Comma))
                        }),
                )
                .build()
        })
        .advance_expecting(consts::B_CLOSE)
        .children(compound)
}

fn p_return<'a, 'b, T: TokenStream<'a>>(builder: NodeBuilder<'a, 'b, T>) -> NodeBuilder<'a, 'b, T> {
    builder
        .ty(NodeType::Return)
        .advance_expecting(consts::RETURN)
        .children(expr)
        .advance_expecting(consts::SEMICOLON)
}

fn assign_or_fun_call<'a, 'b, T: TokenStream<'a>>(
    builder: NodeBuilder<'a, 'b, T>,
) -> NodeBuilder<'a, 'b, T> {
    builder
        .children(identifier)
        .peek_and_type(
            &[Token::Operator(Operator::Assignement), consts::B_OPEN],
            PeekMapping::new()
                .add(
                    Token::Operator(Operator::Assignement),
                    |b: NodeBuilder<'a, 'b, T>| {
                        b.ty(NodeType::Assign)
                            .advance_expecting(Token::Operator(Operator::Assignement))
                            .children(expr)
                    },
                )
                .add(consts::B_OPEN, fun_call_args),
        )
        .advance_expecting(consts::SEMICOLON)
}

fn fun_call_args<'a, 'b, T: TokenStream<'a>>(
    builder: NodeBuilder<'a, 'b, T>,
) -> NodeBuilder<'a, 'b, T> {
    builder
        .ty(NodeType::FunctionCall)
        .advance_expecting(consts::B_OPEN)
        .take_until(consts::B_CLOSE, |it| {
            NodeBuilder::new(NodeType::FunctionCallArgs, it)
                .children(expr)
                .peek_and_type(
                    &[Token::Punctuation(Punctuation::Comma), consts::B_CLOSE],
                    PeekMapping::new()
                        .add(consts::B_CLOSE, |s| s)
                        .add(Token::Punctuation(Punctuation::Comma), |b| {
                            b.advance_expecting(Token::Punctuation(Punctuation::Comma))
                        }),
                )
                .build()
        })
        .advance_expecting(consts::B_CLOSE)
}

fn expr<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new(NodeType::Expr, it)
        .children(term)
        .add_tail(consts::POSSIBLE_EXPR_PRIME_STARTS, expr_prime)
        .build()
}

fn expr_prime<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::POSSIBLE_EXPR_PRIME_STARTS,
            PeekMapping::new()
                .add(consts::AND, |b: NodeBuilder<'a, 'b, T>| b.ty(NodeType::And))
                .add(consts::OR, |b| b.ty(NodeType::Or)),
        )
        .advance_expecting_one_of(consts::POSSIBLE_EXPR_PRIME_STARTS)
        .children(term)
        .add_tail(consts::POSSIBLE_EXPR_PRIME_STARTS, expr_prime)
        .build()
}

fn term<'a>(it: &mut impl TokenStream<'a>) -> Node<'a> {
    NodeBuilder::new(NodeType::Term, it)
        .children(factor)
        .add_tail(consts::POSSIBLE_TERM_PRIME_STARTS, term_prime)
        .build()
}

fn term_prime<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::POSSIBLE_TERM_PRIME_STARTS,
            PeekMapping::new()
                .add(consts::GREAT, |b: NodeBuilder<'a, 'b, T>| {
                    b.ty(NodeType::GreaterThan)
                })
                .add(consts::LESS, |b| b.ty(NodeType::LesserThan))
                .add(consts::GEQ, |b| b.ty(NodeType::GreaterOrEquals))
                .add(consts::LEQ, |b| b.ty(NodeType::LesserOrEquals))
                .add(consts::EQ, |b| b.ty(NodeType::Equals)),
        )
        .advance_expecting_one_of(consts::POSSIBLE_TERM_PRIME_STARTS)
        .children(factor)
        .add_tail(consts::POSSIBLE_TERM_PRIME_STARTS, term_prime)
        .build()
}

fn factor<'a>(it: &mut impl TokenStream<'a>) -> Node<'a> {
    NodeBuilder::new(NodeType::Factor, it)
        .children(product)
        .add_tail(consts::POSSIBLE_FACTOR_PRIME_STARTS, factor_prime)
        .build()
}

fn factor_prime<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::POSSIBLE_FACTOR_PRIME_STARTS,
            PeekMapping::new()
                .add(consts::PLUS, |b: NodeBuilder<'a, 'b, T>| {
                    b.ty(NodeType::Plus)
                })
                .add(consts::MINUS, |b| b.ty(NodeType::Minus)),
        )
        .advance_expecting_one_of(consts::POSSIBLE_FACTOR_PRIME_STARTS)
        .children(product)
        .add_tail(consts::POSSIBLE_FACTOR_PRIME_STARTS, factor_prime)
        .build()
}

fn product<'a>(it: &mut impl TokenStream<'a>) -> Node<'a> {
    NodeBuilder::new(NodeType::Product, it)
        .children(atom)
        .add_tail(consts::POSSIBLE_PRODUCT_PRIME_STARTS, product_prime)
        .build()
}

fn product_prime<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::POSSIBLE_PRODUCT_PRIME_STARTS,
            PeekMapping::new()
                .add(consts::TIMES, |b: NodeBuilder<'a, 'b, T>| {
                    b.ty(NodeType::Times)
                })
                .add(consts::DIVIDE, |b| b.ty(NodeType::Divide)),
        )
        .advance_expecting_one_of(consts::POSSIBLE_PRODUCT_PRIME_STARTS)
        .children(atom)
        .add_tail(consts::POSSIBLE_PRODUCT_PRIME_STARTS, product_prime)
        .build()
}

fn atom<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::POSSIBLE_ATOM_START,
            PeekMapping::new()
                .add(consts::NOT, |b: NodeBuilder<'a, 'b, T>| {
                    b.ty(NodeType::Not)
                        .advance_expecting(consts::NOT)
                        .children(atom)
                })
                .add_all(consts::POSSIBLE_UNIT_STARTS, |b| {
                    b.ty(NodeType::Unit).children(unit)
                }),
        )
        .build()
}

fn unit<'a, 'b, T: TokenStream<'a>>(it: &'b mut T) -> Node<'a> {
    NodeBuilder::new_untyped(it)
        .peek_and_type(
            consts::POSSIBLE_UNIT_STARTS,
            PeekMapping::new()
                .add(consts::B_OPEN, |b: NodeBuilder<'a, 'b, T>| {
                    b.ty(NodeType::BracketedExpr)
                        .advance_expecting(consts::B_OPEN)
                        .children(expr)
                        .advance_expecting(consts::B_CLOSE)
                })
                .add(consts::STRING, |b| {
                    b.ty(NodeType::Str).advance_expecting(consts::STRING)
                })
                .add(consts::ID, |b| {
                    // If we find a bracket we have a fun call, else just identifier
                    b.advance_expecting(consts::ID).peek_if_else(
                        &[consts::B_OPEN],
                        fun_call_args,
                        |b| b.ty(NodeType::Identifier),
                    )
                })
                .add(consts::INT, |b| {
                    let (b, tok) = b.advance_expecting_and_get(consts::INT);
                    b.ty(match tok {
                        Token::Literal(Literal::Integer(i)) => NodeType::Int(i),
                        // This should only happen if the type was already set
                        _ => panic!("Something"),
                    })
                })
                .add(consts::BOOL, |b| {
                    let (b, tok) = b.advance_expecting_and_get(consts::BOOL);
                    b.ty(match tok {
                        Token::Literal(Literal::Boolean(b)) => NodeType::Bool(b),
                        // This should only happen if the type was already set
                        _ => panic!("Something"),
                    })
                }),
        )
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    fn make_tokens_from_str(s: &str) -> impl Iterator<Item = RangedObject<Token>> {
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
    fn parse_precedence() {
        let input = "program fib begin var x := not 1 * 2 + 3 < 4 and 5; end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_same_op() {
        let input = "program fib begin var x := a - b - c; end";
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
    fn parse_fun_call() {
        let input = r#"
        program x
        begin
            f();
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
