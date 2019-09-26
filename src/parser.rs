use crate::lexical::EnrichedToken;
use crate::parsing_utils::*;
use crate::syntax::*;
use crate::tokens::*;

pub(crate) type ExpToken = Token<'static>;
pub(crate) type TokenList = &'static [ExpToken];

pub enum SyntaxError<'a> {
    ExpectedOneOfButFoundEOF(TokenList),
    ExpectedButFoundEOF(ExpToken),
    ExpectedFound(ExpToken, EnrichedToken<'a>),
    ExpectedOneOfFound(TokenList, EnrichedToken<'a>),
    LogicalError,
}

pub(crate) const ID: ExpToken = Token::Identifier("");

pub fn program<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Program<'a>, SyntaxError<'a>> {
    advance_expecting(it, Token::Keyword(Keyword::Program))?;
    let id = advance_expecting_identifier(it)?;
    let compound = compound(it)?;

    Ok(Program { id, compound })
}

fn compound<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Compound<'a>, SyntaxError<'a>> {
    advance_expecting(it, Token::Keyword(Keyword::Begin))?;
    const END: ExpToken = Token::Keyword(Keyword::End);
    let mut statements = Vec::new();
    let mut it = it.peekable();
    loop {
        if let Some(tok) = it.peek() {
            let token = tok.token();
            if token.same_kind(&END) {
                break;
            }

            statements.push(statement(&mut it)?);
        } else {
            return Err(SyntaxError::ExpectedButFoundEOF(END));
        }
    }

    Ok(Compound { statements })
}

fn statement<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Statement<'a>, SyntaxError<'a>> {
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

fn decl<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Decl<'a>, SyntaxError<'a>> {
    let id = advance_expecting_identifier(it)?;
    let mut it = it.peekable();
    let next = it.peek();

    match next.map(|s| s.token()) {
        Some(Token::Punctuation(Punctuation::Semicolon)) => Ok(Decl { id, expr: None }),
        _ => Ok(Decl { id, expr: Some(expr(&mut it)?)})
    }
}
 
fn p_if<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<If<'a>, SyntaxError<'a>> {
    advance_expecting(it, Token::Punctuation(Punctuation::BracketOpen))?;
    let condition = p_bool(it)?;
    advance_expecting(it, Token::Punctuation(Punctuation::BracketClose))?;
    advance_expecting(it, Token::Keyword(Keyword::Then))?;
    let if_branch = compound(it)?;
    let next = advance_expecting_one_of(it, &[Token::Keyword(Keyword::Else), Token::Punctuation(Punctuation::Semicolon)])?;

    let else_branch = match next.token() {
        Token::Keyword(Keyword::Else) => {
            let tmp = compound(it)?;
            advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;
            Some(tmp)
        },
        _ => None
    };

    Ok(If {
        condition,
        if_branch,
        else_branch
    })
}

fn print<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
    token: Token<'a>
) -> Result<Print<'a>, SyntaxError<'a>> {
    match token {
        Token::Keyword(Keyword::Print) => Ok(Print::Print(expr(it)?)),
        Token::Keyword(Keyword::Println) => Ok(Print::Println(expr(it)?)),
        Token::Keyword(Keyword::Get) => Ok(Print::Get(advance_expecting_identifier(it)?)),
        // Should only be called with normal stuff
        _ => Err(SyntaxError::LogicalError)
    }
}

fn p_while<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<While<'a>, SyntaxError<'a>> {
    advance_expecting(it, Token::Punctuation(Punctuation::BracketOpen))?;
    let condition = p_bool(it)?;
    advance_expecting(it, Token::Punctuation(Punctuation::BracketClose))?;
    let compound = compound(it)?;
    advance_expecting(it, Token::Punctuation(Punctuation::Semicolon))?;

    Ok(While {
        condition, 
        compound
    })
}
 
fn assign<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
    id: Identifier<'a>
) -> Result<Assign<'a>, SyntaxError<'a>> {
    advance_expecting(it, Token::Operator(Operator::Assignement))?;
    let expr = expr(it)?;

    Ok(Assign {
        id,
        expr
    })
}
 
fn expr<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Expr<'a>, SyntaxError<'a>> {
    Err(SyntaxError::LogicalError)
}

fn p_bool<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Bool<'a>, SyntaxError<'a>> {
    Err(SyntaxError::LogicalError)
}