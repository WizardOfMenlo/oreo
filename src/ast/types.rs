//! Module for type derivation

use super::node_db::{NodeDb, NodeId};
use super::scope_resolution::VariableResolver;
use super::symbol::SymbolTable;
use super::syntax::*;
use super::IdentId;
use std::collections::HashMap;

// TODO: The error handling right now is absolutely atrocious, try to make it better

/// The types that a variable can have
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// Not derive yet
    Unspecified,

    /// A integer type
    Int,

    /// A boolean type
    Bool,

    /// A string type
    Str,
}

/// A function typing
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    args: Vec<Type>,
    // We use None for void
    out: Option<Type>,
}

/// All type information for a program
#[derive(Debug)]
pub struct Typings {
    /// Temp
    pub types: HashMap<IdentId, Type>,
    ///Temp
    pub funcs: HashMap<IdentId, FuncType>,
}

/// Possible errors in type res
#[derive(Debug)]
pub enum TypeError {
    /// We expect t1 got t2
    ExpectedFoundAt(Type, Type, NodeId),

    /// Expected one of
    ExpectedOneOfAt(&'static [Type], Type, NodeId),

    /// Something was set before being declared
    AssignedBeforeDeclared(IdentId),

    /// Read before being set
    ReadBeforeSet(IdentId),

    /// Expecting $0 args got $1
    FunctionCallExpected(usize, usize),

    /// Function args can't be void
    ExpectedTypeFoundVoid(NodeId),

    /// This should have been a function
    ExpectedFunctionFound(NodeId),

    /// The function needed to return void, but it doesn't
    VoidFunctionWithReturn(NodeId),
}

/// A struct to build typings
pub struct TypingsBuilder<'a, 'b, 'c, 'd> {
    resolver: &'b VariableResolver,
    symbols: &'c SymbolTable<'a>,
    db: &'d NodeDb<'a>,
    types: TypeTable,
    funcs: FuncTable,
    errors: Vec<TypeError>,
}

type TypeTable = HashMap<IdentId, Type>;
type FuncTable = HashMap<IdentId, FuncType>;

impl<'a, 'b, 'c, 'd> TypingsBuilder<'a, 'b, 'c, 'd> {
    /// Creates a new builder
    pub fn new(
        resolver: &'b VariableResolver,
        symbols: &'c SymbolTable<'a>,
        db: &'d NodeDb<'a>,
    ) -> Self {
        Self {
            resolver,
            symbols,
            db,
            types: HashMap::new(),
            funcs: HashMap::new(),
            errors: Vec::new(),
        }
    }

    /// Builds the typer
    pub fn build(mut self, progr: Program) -> Result<Typings, Vec<TypeError>> {
        self.compound(progr.compound(self.db));

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(Typings {
                types: self.types,
                funcs: self.funcs,
            })
        }
    }

    fn unit_type(&mut self, u: Unit) -> Type {
        match u.downcast(self.db) {
            UnitType::Bool(_) => Type::Bool,
            UnitType::Int(_) => Type::Int,
            UnitType::Str(_) => Type::Str,
            UnitType::BracketedExpr(e) => self.typed(e),
            UnitType::FunctionCall(f) => {
                self.resolve_func_call(f);
                let f_id = self.resolver.get_id(f.id(self.db));
                let f_ty = self.funcs.get(&f_id);
                // If we found the function type
                if let Some(t) = f_ty {
                    // If the function does not return void
                    if let Some(t) = t.out {
                        t
                    } else {
                        self.errors
                            .push(TypeError::ExpectedTypeFoundVoid(f.get_id()));
                        Type::Unspecified
                    }
                } else {
                    self.errors.push(TypeError::AssignedBeforeDeclared(f_id));
                    Type::Unspecified
                }
            }
            UnitType::Identifier(i) => {
                let id = self.resolver.get_id(i);
                let possible_ty = self.types.get(&id);
                if let Some(t) = possible_ty {
                    if *t == Type::Unspecified {
                        self.errors.push(TypeError::ReadBeforeSet(id));
                    }
                    *t
                } else {
                    self.errors.push(TypeError::AssignedBeforeDeclared(id));
                    Type::Unspecified
                }
            }
        }
    }

    fn atom_type(&mut self, a: Atom) -> Type {
        match a.downcast(self.db) {
            AtomType::Unit(u) => self.unit_type(u),
            AtomType::Not(a) => {
                let inner_ty = self.atom_type(a);
                if inner_ty != Type::Bool {
                    self.errors
                        .push(TypeError::ExpectedFoundAt(Type::Bool, inner_ty, a.get_id()));
                }
                Type::Bool
            }
        }
    }

    fn product_type(&mut self, p: Product) -> Type {
        let head = p.head(self.db);
        let head_ty = self.atom_type(head);
        let tail = p.tail(self.db);
        if tail.is_none() {
            return head_ty;
        }

        if head_ty != Type::Int {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Int,
                head_ty,
                head.get_id(),
            ));
        }

        let tail = tail.unwrap();
        let tail_ty = self.product_tail(tail);

        if tail_ty != Type::Int {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Int,
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Int
    }

    fn product_tail(&mut self, p: ProductPrime) -> Type {
        let rhs = p.rhs(self.db);
        let rhs_ty = self.atom_type(rhs);
        let tail = p.tail(self.db);
        if tail.is_none() {
            return rhs_ty;
        }

        if rhs_ty != Type::Int {
            self.errors
                .push(TypeError::ExpectedFoundAt(Type::Int, rhs_ty, rhs.get_id()));
        }

        let tail = tail.unwrap();
        let tail_ty = self.product_tail(tail);

        if tail_ty != Type::Int {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Int,
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Int
    }

    fn factor_type(&mut self, p: Factor) -> Type {
        let head = p.head(self.db);
        let head_ty = self.product_type(head);
        let tail = p.tail(self.db);
        if tail.is_none() {
            return head_ty;
        }

        if head_ty != Type::Int {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Int,
                head_ty,
                head.get_id(),
            ));
        }

        let tail = tail.unwrap();
        let tail_ty = self.factor_tail(tail);

        if tail_ty != Type::Int {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Int,
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Int
    }

    fn factor_tail(&mut self, p: FactorPrime) -> Type {
        let rhs = p.rhs(self.db);
        let rhs_ty = self.product_type(rhs);
        let tail = p.tail(self.db);
        if tail.is_none() {
            return rhs_ty;
        }

        if rhs_ty != Type::Int {
            self.errors
                .push(TypeError::ExpectedFoundAt(Type::Int, rhs_ty, rhs.get_id()));
        }

        let tail = tail.unwrap();
        let tail_ty = self.factor_tail(tail);

        if tail_ty != Type::Int {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Int,
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Int
    }

    fn term_type(&mut self, p: Term) -> Type {
        let head = p.head(self.db);
        let head_ty = self.factor_type(head);
        let tail = p.tail(self.db);
        if tail.is_none() {
            return head_ty;
        }

        if head_ty != Type::Int {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Int,
                head_ty,
                head.get_id(),
            ));
        }

        let tail = tail.unwrap();
        let tail_ty = self.term_tail(tail);

        if tail_ty != Type::Int && tail_ty != Type::Bool {
            self.errors.push(TypeError::ExpectedOneOfAt(
                &[Type::Int, Type::Bool],
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Bool
    }

    fn term_tail(&mut self, p: TermPrime) -> Type {
        let rhs = p.rhs(self.db);
        let rhs_ty = self.factor_type(rhs);
        let tail = p.tail(self.db);
        if tail.is_none() {
            return rhs_ty;
        }

        if rhs_ty != Type::Int {
            self.errors
                .push(TypeError::ExpectedFoundAt(Type::Int, rhs_ty, rhs.get_id()));
        }

        let tail = tail.unwrap();
        let tail_ty = self.term_tail(tail);

        if tail_ty != Type::Int && tail_ty != Type::Bool {
            self.errors.push(TypeError::ExpectedOneOfAt(
                &[Type::Int, Type::Bool],
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Bool
    }

    fn expr_tail(&mut self, p: ExprPrime) -> Type {
        let rhs = p.rhs(self.db);
        let rhs_ty = self.term_type(rhs);
        let tail = p.tail(self.db);
        if tail.is_none() {
            return rhs_ty;
        }

        if rhs_ty != Type::Bool {
            self.errors
                .push(TypeError::ExpectedFoundAt(Type::Bool, rhs_ty, rhs.get_id()));
        }

        let tail = tail.unwrap();
        let tail_ty = self.expr_tail(tail);

        if tail_ty != Type::Bool {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Bool,
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Bool
    }

    fn typed(&mut self, e: Expr) -> Type {
        let head = e.head(self.db);
        let head_ty = self.term_type(head);
        let tail = e.tail(self.db);
        if tail.is_none() {
            return head_ty;
        }

        if head_ty != Type::Bool {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Bool,
                head_ty,
                head.get_id(),
            ));
        }

        let tail = tail.unwrap();
        let tail_ty = self.expr_tail(tail);

        if tail_ty != Type::Bool {
            self.errors.push(TypeError::ExpectedFoundAt(
                Type::Bool,
                tail_ty,
                tail.get_id(),
            ));
        }

        Type::Bool
    }

    fn type_condition(&mut self, e: Expr) {
        let ty = self.typed(e);
        if ty != Type::Bool {
            self.errors
                .push(TypeError::ExpectedFoundAt(Type::Bool, ty, e.get_id()))
        }
    }

    // Resolve the func call and get the out type
    fn resolve_func_call(&mut self, f: FunctionCall) {
        use super::symbol::DeclarationContext;
        // Get the id
        let id = f.id(self.db);
        let ident_id = self.resolver.get_id(id);

        // Have we resolved it already?
        if !self.funcs.contains_key(&ident_id) {
            // If not get the function declartion
            let var_record = self
                .symbols
                .get_variable_record(ident_id)
                .expect("Could not find in symbol");

            // And go resolve it
            if let DeclarationContext::FunctionDecl(decl) = var_record.decl() {
                self.resolve_func_decl(*decl);
            } else {
                self.errors
                    .push(TypeError::ExpectedFunctionFound(id.get_id()));
                return;
            }
        }

        // Get the type
        let fun_type = self.funcs.get(&ident_id).unwrap().clone();

        // Check args len
        let args = f.args(self.db);
        if fun_type.args.len() != args.len() {
            self.errors.push(TypeError::FunctionCallExpected(
                fun_type.args.len(),
                args.len(),
            ));
            return;
        }

        // Check args types
        for (ty, arg) in fun_type.args.iter().zip(args) {
            let expr = arg.expr(self.db);
            let e_ty = self.typed(expr);
            if e_ty != *ty {
                self.errors
                    .push(TypeError::ExpectedFoundAt(*ty, e_ty, arg.get_id()));
            }
        }
    }

    fn resolve_func_decl(&mut self, f: FunctionDecl) {
        use crate::lexer::tokens;
        // Get the id
        let id = self.resolver.get_id(f.id(self.db));

        // Get the return type
        let ret_ty = f.return_ty(self.db).typed(self.db);
        let final_ret_ty = match ret_ty {
            tokens::Type::Void => None,
            tokens::Type::Boolean => Some(Type::Bool),
            tokens::Type::Integer => Some(Type::Int),
            tokens::Type::Str => Some(Type::Str),
        };

        // For each arguments
        let args = f.args(self.db);
        let mut res_args = Vec::new();
        for arg in args {
            let arg_id = self.resolver.get_id(arg.id(self.db));
            // Get the declared type
            let arg_ty = arg.typed(self.db).typed(self.db);

            // Convert it to the repr type
            let res_ty = match arg_ty {
                tokens::Type::Void => {
                    self.errors
                        .push(TypeError::ExpectedTypeFoundVoid(arg.get_id()));
                    Type::Unspecified
                }
                tokens::Type::Boolean => Type::Bool,
                tokens::Type::Integer => Type::Int,
                tokens::Type::Str => Type::Str,
            };

            if res_ty == Type::Unspecified {
                continue;
            }

            self.types.insert(arg_id, res_ty);

            // Add to the signature
            res_args.push(res_ty);
        }

        // Validate the body
        let body = f.compound(self.db);
        self.compound(body);

        // Check every return statement for well typed ness
        // TODO: Handle none case
        if let Some(t) = final_ret_ty {
            for s in body.statements(self.db) {
                match s.downcast(self.db) {
                    // Only vet return statemment
                    StatementType::Return(r) => {
                        let ty = self.typed(r.expr(self.db));
                        if ty != t {
                            self.errors
                                .push(TypeError::ExpectedFoundAt(t, ty, r.get_id()));
                        }
                    }
                    _ => continue,
                }
            }
        } else if body
            .statements(self.db)
            .iter()
            .map(|s| s.downcast(self.db))
            .any(|s| match s {
                StatementType::Return(_) => true,
                _ => false,
            })
        {
            self.errors
                .push(TypeError::VoidFunctionWithReturn(body.get_id()));
        }

        // Insert the signature
        self.funcs.insert(
            id,
            FuncType {
                out: final_ret_ty,
                args: res_args,
            },
        );
    }

    fn compound(&mut self, compound: Compound) {
        for statement in compound.statements(self.db) {
            match statement.downcast(self.db) {
                StatementType::Decl(d) => {
                    // Insert the id into the mapping
                    let new_var = self.resolver.get_id(d.id(self.db));

                    // If the type can be deduced insert it, else leave it unspecified
                    let res_ty = if let Some(e) = d.expr(self.db) {
                        self.typed(e)
                    } else {
                        Type::Unspecified
                    };

                    self.types.insert(new_var, res_ty);
                }
                StatementType::Assign(a) => {
                    // Get the id
                    let old_var = self.resolver.get_id(a.id(self.db));
                    let expr_ty = self.typed(a.expr(self.db));
                    // If we are here the thing was not declared
                    if !self.types.contains_key(&old_var) {
                        self.errors.push(TypeError::AssignedBeforeDeclared(old_var));
                    } else {
                        // Get the old
                        let entry = self.types.get_mut(&old_var).unwrap();
                        // If we didn't have a type set it
                        if *entry == Type::Unspecified {
                            *entry = expr_ty
                        }
                        // Else check that they are the same type
                        else if *entry != expr_ty {
                            self.errors.push(TypeError::ExpectedFoundAt(
                                *entry,
                                expr_ty,
                                a.get_id(),
                            ))
                        }
                    }
                }
                StatementType::FunctionCall(f) => self.resolve_func_call(f),
                StatementType::FunctionDecl(f) => self.resolve_func_decl(f),
                StatementType::If(i) => {
                    self.type_condition(i.condition(self.db));
                    self.compound(i.if_branch(self.db));
                    if let Some(e) = i.else_branch(self.db) {
                        self.compound(e)
                    }
                }
                StatementType::While(w) => {
                    self.type_condition(w.condition(self.db));
                    self.compound(w.compound(self.db));
                }
                StatementType::Return(r) => {
                    self.typed(r.expr(self.db));
                }
                StatementType::PrintStat(p) => match p.downcast(self.db) {
                    PrintTypes::Get(g) => {
                        let id = self.resolver.get_id(g.id(self.db));
                        let ty = self.types.get(&id);
                        match ty {
                            Some(Type::Str) | Some(Type::Unspecified) => {
                                self.types.insert(id, Type::Str);
                            }
                            Some(Type::Int) | Some(Type::Bool) => self.errors.push(
                                TypeError::ExpectedFoundAt(Type::Str, *ty.unwrap(), g.get_id()),
                            ),
                            None => {
                                self.errors.push(TypeError::AssignedBeforeDeclared(id));
                            }
                        }
                    }
                    PrintTypes::Print(p) => {
                        self.typed(p.expr(self.db));
                    }
                    PrintTypes::Println(p) => {
                        self.typed(p.expr(self.db));
                    }
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::node_db::NodeDbWrap;
    use super::super::scope_resolution::VariableResolver;
    use super::super::symbol::SymbolTable;
    use insta::assert_debug_snapshot;

    fn db_from_str(input: &str) -> NodeDbWrap {
        use crate::lexer::lexicalize;
        use crate::lexer::scanner::scan;
        use crate::parser::parse;

        let node = parse(lexicalize(scan(input)));
        NodeDb::new(node)
    }

    fn sym_table_from<'a>(input: &'a str, db: &NodeDb<'a>) -> SymbolTable<'a> {
        use super::super::symbol::SymbolTableBuilder;
        SymbolTableBuilder::new(input, db).build(Program::new(db.start_id()))
    }

    fn resolver<'a>(input: &'a str, db: &NodeDb<'a>, sym: &SymbolTable<'a>) -> VariableResolver {
        use super::super::scope_resolution::VariableResolverBuilder;
        VariableResolverBuilder::new(input, sym, db)
            .build(Program::new(db.start_id()))
            .unwrap()
    }

    type IdentVec = Vec<(IdentId, super::Type)>;
    type FuncVec = Vec<(IdentId, FuncType)>;

    fn determinize(ty: Typings) -> (IdentVec, FuncVec) {
        let mut types: Vec<_> = ty.types.into_iter().collect();
        let mut funcs: Vec<_> = ty.funcs.into_iter().collect();
        types.sort_by(|a, b| (a.0).0.cmp(&(b.0).0));
        funcs.sort_by(|a, b| (a.0).0.cmp(&(b.0).0));

        (types, funcs)
    }

    fn test_input(input: &str) -> Result<(IdentVec, FuncVec), Vec<TypeError>> {
        let db = db_from_str(input);
        let sym = sym_table_from(input, &db);
        let resolver = resolver(input, &db, &sym);
        let types = TypingsBuilder::new(&resolver, &sym, &db).build(Program::new(db.start_id()));
        types.map(determinize)
    }

    #[test]
    fn type_valid_simple_int() {
        let input = "program x begin var x := 1; end";
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_valid_simple_bool() {
        let input = "program x begin var x := true; end";
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_valid_simple_str() {
        let input = r#"program x begin var x := "Hello"; end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_arithmetic() {
        let input = r#"program x begin var x := 1 + 2 * 3 / 5; end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_bool_arithmetic() {
        let input = r#"program x begin var x := not true and false or false; end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_bool_arithmetic_relation() {
        let input = r#"program x begin var x := 1 < 2 or 3 < 4; end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_easy_variable_inf() {
        let input = r#"program x begin var x := 1; var y := x; end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_invalid_operation() {
        let input = r#"program x begin var x := 1 + "hello"; end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_invalid_operation_with_steps() {
        let input = r#"program x begin var x := 1; var y := x + "hello"; end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_fun_decl() {
        let input = r#"program x
        begin
            procedure int f(var x ~ int)
            begin
                return x;
            end
        end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_fun_decl_invalid_return() {
        let input = r#"program x
        begin
            procedure int f(var x ~ str)
            begin
                return x;
            end
        end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_fun_decl_void_return() {
        let input = r#"program x
        begin
            procedure void f(var x ~ str)
            begin
                var y := x;
            end
        end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_fun_decl_void_invalid_return() {
        let input = r#"program x
        begin
            procedure void f(var x ~ str)
            begin
                return x;
            end
        end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_fun_decl_call_in_expr() {
        let input = r#"program x
        begin
            procedure int f(var x ~ int)
            begin
                return x;
            end

            var x := f(1);
        end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_fun_call_in_expr_invalid_arg_ty() {
        let input = r#"program x
        begin
            procedure int f(var x ~ int)
            begin
                return x;
            end

            var x := f("hello");
        end"#;
        assert_debug_snapshot!(test_input(input));
    }

    #[test]
    fn type_fun_call_in_expr_invalid_arg_num() {
        let input = r#"program x
        begin
            procedure int f(var x ~ int)
            begin
                return x;
            end

            var x := f(1, 2);
        end"#;
        assert_debug_snapshot!(test_input(input));
    }
}
