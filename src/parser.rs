use crate::lexical::EnrichedToken;
use crate::parsing_utils::*;
use crate::syntax::*;
use crate::tokens::*;
use std::iter::Peekable;

pub(crate) type ExpToken = Token<'static>;
pub(crate) type TokenList = &'static [ExpToken];

#[derive(Debug)]
pub enum SyntaxError<'a> {
    ExpectedOneOfButFoundEOF(TokenList),
    ExpectedButFoundEOF(ExpToken),
    ExpectedFound(ExpToken, EnrichedToken<'a>),
    ExpectedOneOfFound(TokenList, EnrichedToken<'a>),
    LogicalError,
}

pub(crate) const ID: ExpToken = Token::Identifier("");

pub fn program<'a, T>(it: &mut Peekable<T>) -> Result<Program<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    advance_expecting(it, Token::Keyword(Keyword::Program))?;
    let id = advance_expecting_identifier(it)?;
    advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;
    let compound = compound(it)?;

    Ok(Program { id, compound })
}

pub fn compound<'a, T>(it: &mut Peekable<T>) -> Result<Compound<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    advance_expecting(it, Token::Keyword(Keyword::Begin))?;
    const END: ExpToken = Token::Keyword(Keyword::End);
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

pub fn statement<'a, T>(it: &mut Peekable<T>) -> Result<Statement<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    const POSSIBLE_STATEMENT_STARTS: TokenList = &[
        Token::Keyword(Keyword::Var),
        Token::Keyword(Keyword::If),
        Token::Keyword(Keyword::Print),
        Token::Keyword(Keyword::Println),
        Token::Keyword(Keyword::Get),
        Token::Keyword(Keyword::While),
        ID,
    ];

    let matched = advance_expecting_one_of(it, POSSIBLE_STATEMENT_STARTS)?;
    let token = matched.take_token();
    match token {
        Token::Keyword(Keyword::Var) => Ok(Statement::Decl(decl(it)?)),
        Token::Keyword(Keyword::If) => Ok(Statement::If(p_if(it)?)),
        Token::Keyword(Keyword::Print) => Ok(Statement::Print(print(it, token)?)),
        Token::Keyword(Keyword::Println) => Ok(Statement::Print(print(it, token)?)),
        Token::Keyword(Keyword::Get) => Ok(Statement::Print(print(it, token)?)),
        Token::Keyword(Keyword::While) => Ok(Statement::While(p_while(it)?)),
        Token::Identifier(s) => Ok(Statement::Assign(assign(it, Identifier(s))?)),
        _ => Err(SyntaxError::LogicalError),
    }
}

pub fn decl<'a, T>(it: &mut Peekable<T>) -> Result<Decl<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let id = advance_expecting_identifier(it)?;
    let next = advance_expecting_one_of(
        it,
        &[
            Token::Punctuation(Punctuation::Semicolon),
            Token::Operator(Operator::Assignement),
        ],
    )?;

    Ok(match next.token() {
        Token::Punctuation(Punctuation::Semicolon) => Decl { id, expr: None },
        Token::Operator(Operator::Assignement) => {
            let expr = Some(expr(it)?);
            advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;
            Decl { id, expr }
        }
        _ => return Err(SyntaxError::LogicalError),
    })
}

pub fn p_if<'a, T>(it: &mut Peekable<T>) -> Result<If<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    advance_expecting(it, Token::Punctuation(Punctuation::BracketOpen))?;
    let condition = p_bool(it)?;
    advance_expecting(it, Token::Punctuation(Punctuation::BracketClose))?;
    advance_expecting(it, Token::Keyword(Keyword::Then))?;
    let if_branch = compound(it)?;
    let next = advance_expecting_one_of(
        it,
        &[
            Token::Keyword(Keyword::Else),
            Token::Punctuation(Punctuation::Semicolon),
        ],
    )?;

    let else_branch = match next.token() {
        Token::Keyword(Keyword::Else) => {
            let tmp = compound(it)?;
            advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;
            Some(tmp)
        }
        _ => None,
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
    let print_stat = match token {
        Token::Keyword(Keyword::Print) => Print::Print(expr(it)?),
        Token::Keyword(Keyword::Println) => Print::Println(expr(it)?),
        Token::Keyword(Keyword::Get) => Print::Get(advance_expecting_identifier(it)?),
        // Should only be called with normal stuff
        _ => return Err(SyntaxError::LogicalError),
    };

    advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;

    Ok(print_stat)
}

fn p_while<'a, T>(it: &mut Peekable<T>) -> Result<While<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    advance_expecting(it, Token::Punctuation(Punctuation::BracketOpen))?;
    let condition = p_bool(it)?;
    advance_expecting(it, Token::Punctuation(Punctuation::BracketClose))?;
    let compound = compound(it)?;
    advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;

    Ok(While {
        condition,
        compound,
    })
}
fn assign<'a, T>(it: &mut Peekable<T>, id: Identifier<'a>) -> Result<Assign<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    advance_expecting(it, Token::Operator(Operator::Assignement))?;
    let expr = expr(it)?;

    Ok(Assign { id, expr })
}

fn expr<'a, T>(it: &mut Peekable<T>) -> Result<Expr<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let head = expr_head(it)?;
    let next = it.peek();
    let tail = match next.map(|t| t.token()) {
        Some(Token::Operator(Operator::Plus))
        | Some(Token::Operator(Operator::Minus))
        | Some(Token::Operator(Operator::Times))
        | Some(Token::Operator(Operator::Divide)) => Some(Box::new(expr_prime(it)?)),
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
        Some(Token::Punctuation(Punctuation::BracketOpen)) => {
            advance_expecting(it, Token::Punctuation(Punctuation::BracketOpen))?;
            let expr = expr(it)?;
            advance_expecting(it, Token::Punctuation(Punctuation::BracketClose))?;
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

const INT: Token<'static> = Token::Literal(Literal::Integer(0));
const STRING: Token<'static> = Token::Literal(Literal::String(""));

fn unit<'a, T>(it: &mut Peekable<T>) -> Result<Unit<'a>, SyntaxError<'a>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    let next = advance_expecting_one_of(it, &[INT, STRING, ID])?;
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
    let operator = advance_expecting_one_of(
        it,
        &[
            Token::Operator(Operator::Plus),
            Token::Operator(Operator::Minus),
            Token::Operator(Operator::Times),
            Token::Operator(Operator::Divide),
        ],
    )?;

    let operand = expr(it)?;

    let tail = match it.peek().map(|t| t.token()) {
        Some(Token::Operator(Operator::Plus))
        | Some(Token::Operator(Operator::Minus))
        | Some(Token::Operator(Operator::Times))
        | Some(Token::Operator(Operator::Divide)) => Some(Box::new(expr_prime(it)?)),
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
    use std::iter::Peekable;

    fn make_tokens_from_str(s: &str) -> Peekable<impl Iterator<Item = EnrichedToken>> {
        use crate::lexical::lexicalize;
        use crate::scanner::scan;

        lexicalize(scan(s)).peekable()
    }

    #[test]
    fn parse_empty() {
        let parsed = program(&mut make_tokens_from_str(""));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_invalid() {
        let parsed = program(&mut make_tokens_from_str("(x <= > >= < y 99.88l8 )"));
        assert_debug_snapshot!(parsed)
    }

    #[test]
    fn parse_minimal_no_bool_no_expr() {
        let input = "program fib; begin var n; end";
        let parsed = program(&mut make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_minimal_string_assign() {
        let input = "program fib; begin var n := \"Hello\"; end";
        let parsed = program(&mut make_tokens_from_str(input));
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn parse_full() {
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let parsed = program(&mut make_tokens_from_str(input));
        assert_debug_snapshot!(parsed)
    }
}
