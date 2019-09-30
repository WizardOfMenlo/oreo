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
    advance_expecting(it, consts::SEMICOLON)?;
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

fn expr<'a, T>(it: &mut Peekable<T>) -> Result<Expr<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let head = expr_head(it)?;
    let next = it.peek();
    let tail = match next.map(|t| consts::POSSIBLE_EXPR_PRIME_STARTS.contains(t.token())) {
        Some(true) => Some(Box::new(expr_prime(it)?)),
        _ => None,
    };

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
        Token::Identifier(s) => Unit::Identifier(Identifier(s)),
        _ => return Err(SyntaxError::LogicalError),
    })
}

fn expr_prime<'a, T>(it: &mut Peekable<T>) -> Result<ExprPrime<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let operator = advance_expecting_one_of(it, consts::POSSIBLE_EXPR_PRIME_STARTS)?;

    let operand = expr(it)?;

    let tail = match it
        .peek()
        .map(|t| consts::POSSIBLE_EXPR_PRIME_STARTS.contains(t.token()))
    {
        Some(true) => Some(Box::new(expr_prime(it)?)),
        _ => None,
    };

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

fn p_bool<'a, T>(it: &mut Peekable<T>) -> Result<Bool<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    Err(SyntaxError::LogicalError)
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
        let input = "program fib; begin var n; end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_minimal_assignments() {
        let input = "program fib; begin var n := \"Hello\"; var m := 2; var s := (2); end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_print_simple() {
        let input = "program fib; begin print \"Hello\\n\"; println 2; get x; end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_no_args() {
        let input = "program fib; begin procedure main() begin return 1; end end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_single_arg() {
        let input = "program fib; begin procedure id(var x) begin return x; end end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_function_mult_arg() {
        let input = "program fib; begin procedure sum(var x, var y) begin return y; end end";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_full() {
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let parsed = parse(make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }
}
