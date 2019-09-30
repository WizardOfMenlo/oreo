use crate::lexical::EnrichedToken;
use crate::parser::{consts, ExpToken, SyntaxError, TokenList};
use crate::syntax::*;
use crate::tokens::*;

#[must_use]
pub fn advance_expecting<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
    exp: ExpToken,
) -> Result<EnrichedToken<'a>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => {
            let found_token = tok.token();
            if found_token.same_kind(&exp) {
                Ok(tok)
            } else {
                Err(SyntaxError::ExpectedFound(exp, tok))
            }
        }
        None => Err(SyntaxError::ExpectedButFoundEOF(exp)),
    }
}

#[must_use]
pub fn advance_expecting_one_of<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
    exp: TokenList,
) -> Result<EnrichedToken<'a>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => {
            let found_token = tok.token();
            if exp.iter().any(|t| found_token.same_kind(t)) {
                Ok(tok)
            } else {
                Err(SyntaxError::ExpectedOneOfFound(exp, tok))
            }
        }
        None => Err(SyntaxError::ExpectedOneOfButFoundEOF(exp)),
    }
}

#[must_use]
pub fn get_id(id: EnrichedToken) -> Result<Identifier, SyntaxError> {
    let backup = id.clone();
    match id.take_token() {
        Token::Identifier(s) => Ok(Identifier(s)),
        _ => Err(SyntaxError::ExpectedFound(consts::ID, backup)),
    }
}

#[must_use]
pub fn advance_expecting_identifier<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
) -> Result<Identifier<'a>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => get_id(tok),
        None => Err(SyntaxError::ExpectedButFoundEOF(consts::ID)),
    }
}
