use crate::syntax::*;

#[derive(Debug, Clone)]
pub enum ValidationError<'a> {
    ExpectedBoolFoundArithmetic,
    ExpectedBoolFound(Unit<'a>),
}

pub fn validate_program<'a>(p: &Program<'a>) -> Result<(), ValidationError<'a>> {
    validate_compound(&p.compound)?;
    Ok(())
}

pub fn validate_compound<'a>(c: &Compound<'a>) -> Result<(), ValidationError<'a>> {
    for s in &c.statements {
        validate_statement(s)?;
    }

    Ok(())
}

pub fn validate_statement<'a>(s: &Statement<'a>) -> Result<(), ValidationError<'a>> {
    match s {
        Statement::While(w) => validate_while(w),
        Statement::If(i) => validate_if(i),
        Statement::Assign(a) => validate_expr(&a.expr),
        Statement::FunctionDecl(d) => validate_compound(&d.inner),
        Statement::Print(p) => validate_print(p),
        Statement::Return(r) => validate_expr(&r.expr),
        _ => Ok(()),
    }
}

pub fn validate_print<'a>(p: &Print<'a>) -> Result<(), ValidationError<'a>> {
    match p {
        Print::Print(e) => validate_expr(e),
        Print::Println(e) => validate_expr(e),
        _ => Ok(()),
    }
}

pub fn validate_while<'a>(w: &While<'a>) -> Result<(), ValidationError<'a>> {
    validate_condition(&w.condition)?;
    validate_compound(&w.compound)?;
    Ok(())
}

pub fn validate_if<'a>(i: &If<'a>) -> Result<(), ValidationError<'a>> {
    validate_condition(&i.condition)?;
    validate_compound(&i.if_branch)?;
    i.else_branch
        .clone()
        .map(|c| validate_compound(&c))
        .unwrap_or(Ok(()))?;
    Ok(())
}

pub fn validate_condition<'a>(i: &Expr<'a>) -> Result<(), ValidationError<'a>> {
    validate_expr(i)?;
    if i.tail.is_none() {
        validate_term_as_condition(&i.head)?
    }

    Ok(())
}

pub fn validate_expr<'a>(i: &Expr<'a>) -> Result<(), ValidationError<'a>> {
    // And/or require left and right to be bools
    validate_term(&i.head)?;
    if i.tail.is_some() {
        validate_term_as_condition(&i.head)?
    }
    let mut tail = i.tail.clone();
    while let Some(t) = tail {
        validate_term_as_condition(&t.rhs)?;
        tail = t.tail.map(|s| *s);
    }

    Ok(())
}

pub fn validate_term<'a>(i: &Term<'a>) -> Result<(), ValidationError<'a>> {
    validate_factor(&i.head)?;
    let mut tail = i.tail.clone();
    while let Some(t) = tail {
        validate_factor(&t.rhs)?;
        tail = t.tail.map(|s| *s);
    }

    Ok(())
}

pub fn validate_term_as_condition<'a>(i: &Term<'a>) -> Result<(), ValidationError<'a>> {
    validate_term(i)?;
    if i.tail.is_some() {
        Ok(())
    } else {
        validate_factor_as_condition(&i.head)
    }
}

pub fn validate_factor<'a>(i: &Factor<'a>) -> Result<(), ValidationError<'a>> {
    validate_product(&i.head)?;
    let mut tail = i.tail.clone();
    while let Some(t) = tail {
        validate_product(&t.rhs)?;
        tail = t.tail.map(|s| *s);
    }

    Ok(())
}

pub fn validate_factor_as_condition<'a>(i: &Factor<'a>) -> Result<(), ValidationError<'a>> {
    validate_factor(i)?;
    // Sum can never give a boolean!
    if i.tail.is_none() {
        validate_product_as_condition(&i.head)
    } else {
        Err(ValidationError::ExpectedBoolFoundArithmetic)
    }
}

pub fn validate_product<'a>(i: &Product<'a>) -> Result<(), ValidationError<'a>> {
    validate_atom(&i.head)?;
    let mut tail = i.tail.clone();
    while let Some(t) = tail {
        validate_atom(&t.rhs)?;
        tail = t.tail.map(|s| *s);
    }
    Ok(())
}

pub fn validate_product_as_condition<'a>(i: &Product<'a>) -> Result<(), ValidationError<'a>> {
    validate_product(i)?;

    // Product can never be boolean!
    if i.tail.is_none() {
        validate_atom_as_condition(&i.head)
    } else {
        Err(ValidationError::ExpectedBoolFoundArithmetic)
    }
}

pub fn validate_atom<'a>(a: &Atom<'a>) -> Result<(), ValidationError<'a>> {
    match a {
        Atom::Unit(u) => validate_unit(u),
        Atom::Not(a) => validate_atom_as_condition(a),
    }
}

pub fn validate_atom_as_condition<'a>(a: &Atom<'a>) -> Result<(), ValidationError<'a>> {
    validate_atom(a)?;
    if let Atom::Unit(u) = a {
        validate_unit_as_condition(u)?
    }

    Ok(())
}

pub fn validate_unit<'a>(a: &Unit<'a>) -> Result<(), ValidationError<'a>> {
    match a {
        Unit::BracketedExpr(e) => validate_expr(e),
        Unit::FunctionCall(f) => validate_func_call(f),
        _ => Ok(()),
    }
}

pub fn validate_unit_as_condition<'a>(a: &Unit<'a>) -> Result<(), ValidationError<'a>> {
    validate_unit(a)?;
    match a {
        Unit::Boolean(_) | Unit::FunctionCall(_) | Unit::Identifier(_) => Ok(()),
        Unit::BracketedExpr(e) => validate_condition(e),
        _ => Err(ValidationError::ExpectedBoolFound(a.clone())),
    }
}

pub fn validate_func_call<'a>(f: &FunctionCall<'a>) -> Result<(), ValidationError<'a>> {
    for arg in &f.args {
        validate_expr(arg)?;
    }

    Ok(())
}
