//! Module with facilities for Three Address Code gen

use crate::ast::types::Type;
use crate::ast::IdentId;
use crate::ast::NodeId;
use crate::ast::AST;
use crate::common;
use std::collections::HashMap;
use std::fmt;

/// A struct for any possible type we use here
#[derive(Debug, Clone, Copy)]
pub enum VettedTy {
    /// Integer (or boolean since bool just a fancy int)
    Int,

    /// String
    Str,
}

impl From<Type> for VettedTy {
    fn from(t: Type) -> Self {
        match t {
            Type::Bool | Type::Int => VettedTy::Int,
            Type::Str => VettedTy::Str,
            Type::Unspecified => panic!("Type res has failed"),
        }
    }
}

/// A type for a label
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Label(usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

/// Any kind of constant
#[derive(Debug, Copy, Clone)]
pub enum Const {
    /// Int
    Int(isize),

    /// Str (referred by node id)
    Str(NodeId),
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::Int(i) => write!(f, "{}", i),
            Const::Str(n) => write!(f, "s{}", n.0),
        }
    }
}

/// One of the allowed operations
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum SimpleOp {
    Multiplicative(common::MultiplicativeOp),
    Additive(common::AdditiveOp),
    Boolean(common::BooleanOp),
    Relational(common::RelationalOp),
    Not,
}

impl fmt::Display for SimpleOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SimpleOp::Additive(common::AdditiveOp::Plus) => "+",
                SimpleOp::Additive(common::AdditiveOp::Minus) => "-",
                SimpleOp::Multiplicative(common::MultiplicativeOp::Times) => "*",
                SimpleOp::Multiplicative(common::MultiplicativeOp::Divide) => "/",
                SimpleOp::Boolean(common::BooleanOp::And) => "and",
                SimpleOp::Boolean(common::BooleanOp::Or) => "or",
                SimpleOp::Relational(common::RelationalOp::Equals) => "==",
                SimpleOp::Relational(common::RelationalOp::LesserOrEquals) => "<=",
                SimpleOp::Relational(common::RelationalOp::LesserThan) => "<",
                SimpleOp::Relational(common::RelationalOp::GreaterOrEquals) => ">=",
                SimpleOp::Relational(common::RelationalOp::GreaterThan) => ">",
                SimpleOp::Not => "not",
            }
        )
    }
}

/// A location in memory
#[derive(Debug, Copy, Clone)]
pub enum MemoryLocation {
    /// An address
    Address(Address),

    /// A static const
    Const(Const),
}

impl fmt::Display for MemoryLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemoryLocation::Address(a) => write!(f, "{}", a),
            MemoryLocation::Const(c) => write!(f, "{}", c),
        }
    }
}

/// We use this for unary ops
const THROWAWAY: MemoryLocation = MemoryLocation::Address(Address::Temp(0));

/// An address in memory
#[derive(Debug, Copy, Clone)]
pub enum Address {
    /// A tempory transparent elem
    Temp(usize),

    /// A id that refers to the same in source code
    Orig(IdentId),
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Address::Temp(i) => write!(f, "t{}", i),
            Address::Orig(o) => write!(f, "o{}", o.0),
        }
    }
}

/// One of the complex builtin ops
#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum Builtin {
    Print,
    Println,
    Get,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Builtin::Print => "_Print",
                Builtin::Println => "_Println",
                Builtin::Get => "_Get",
            }
        )
    }
}

/// An executable instruction
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Jump to label
    Jump(Label),

    /// Return the value of the mem loc
    Return(MemoryLocation, VettedTy),

    /// Jump to label if memory location is (bool)
    ConditionalJump(MemoryLocation, Type, Label, bool),

    /// A instruction like x := a + b
    Simple(SimpleInstruction),

    /// Initialize an address
    Set(Address, MemoryLocation, VettedTy),

    /// Push this to stack
    Push(MemoryLocation, VettedTy),

    /// Pop to an address
    Pop(Address, VettedTy),

    /// Call the function referred here (and store result in Address)
    Call(IdentId, Address, VettedTy),

    /// Call the function referred here (and store result in Address)
    CallNoRet(IdentId),

    /// Call a builtin function with a ptr
    CallBuiltin(Builtin, Address, VettedTy),

    /// A label
    Label(Label),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Jump(l) => write!(f, "jmp {}", l),
            Instruction::Return(m, _) => write!(f, "Return {}", m),
            Instruction::ConditionalJump(m, _, l, b) => {
                write!(f, "jmp {} {}{}", l, if *b { "" } else { "!" }, m)
            }
            Instruction::Set(a, m, _) => write!(f, "{} := {}", a, m),
            Instruction::Push(m, _) => write!(f, "Push {}", m),
            Instruction::Pop(a, _) => write!(f, "Pop {}", a),
            Instruction::Call(i, a, _) => write!(f, "{} := Call f{}", a, i.0),
            Instruction::CallNoRet(i) => write!(f, "Call f{}", i.0),
            Instruction::CallBuiltin(b, a, _) => write!(f, "Call {} {}", b, a),
            Instruction::Simple(simple) => write!(f, "{}", simple),
            Instruction::Label(l) => write!(f, "{}", l),
        }
    }
}

/// An executable intstruction in TAC
#[derive(Debug, Clone)]
pub struct SimpleInstruction {
    out: Address,
    left: MemoryLocation,
    right: MemoryLocation,
    op: SimpleOp,
    ty: VettedTy,
}

impl fmt::Display for SimpleInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} := {} {} {}",
            self.out, self.left, self.op, self.right
        )
    }
}

/// The global TAC scope
#[derive(Debug)]
pub struct GlobalTAC {
    pub(super) program_name: String,
    pub(super) functions: HashMap<IdentId, TAC>,
    pub(super) global: TAC,
}

impl fmt::Display for GlobalTAC {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "program {}", self.program_name)?;
        for (func_id, code) in &self.functions {
            writeln!(f, "BeginFunc f{}", func_id.0)?;
            write!(f, "{}", code)?;
            writeln!(f, "EndFunc")?;
        }
        writeln!(f, "begin {}", self.program_name)?;

        write!(f, "{}", self.global)?;

        writeln!(f, "end {}", self.program_name)?;

        Ok(())
    }
}

/// a list of instructions
#[derive(Debug)]
pub struct TAC {
    pub(super) instructions: Vec<Instruction>,
}

impl fmt::Display for TAC {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?;
        }

        Ok(())
    }
}

/// Utility for building the TAC
#[derive(Debug)]
pub struct TACBuilder<'a, 'b> {
    ast: &'b AST<'a>,
    current_id: usize,
    current_label: usize,
    instructions: Vec<Instruction>,
    functions: HashMap<IdentId, TAC>,
    program_name: String,
}

use crate::ast::syntax::*;
impl<'a, 'b> TACBuilder<'a, 'b> {
    /// Init this builder
    pub fn new(ast: &'b AST<'a>) -> Self {
        Self::new_with_instructions(ast, Vec::new())
    }

    fn new_with_instructions(ast: &'b AST<'a>, instructions: Vec<Instruction>) -> Self {
        TACBuilder {
            ast,
            current_id: 1,
            current_label: 0,
            functions: HashMap::new(),
            instructions,
            program_name: ast.program_name().to_string(),
        }
    }

    /// Build given a program
    pub fn build(self, p: Program) -> GlobalTAC {
        let comp = p.compound(self.ast.db());
        let global = self.build_from_compound(comp);
        GlobalTAC {
            functions: global.0.functions,
            global: global.1,
            program_name: global.0.program_name,
        }
    }

    fn build_from_compound(mut self, p: Compound) -> (Self, TAC) {
        self.compound(p);

        let instructions = self.instructions.clone();

        (self, TAC { instructions })
    }

    fn next_label(&mut self) -> Label {
        self.current_label += 1;
        Label(self.current_label)
    }

    fn next_temp(&mut self) -> Address {
        self.current_id += 1;
        Address::Temp(self.current_id)
    }

    fn declare_function(&mut self, f: FunctionDecl) {
        let id = self.ast.variables().get_id(f.id(self.ast.db()));
        let mut prov_instr = Vec::new();
        // Note we reverse here the order
        for arg in f.args(self.ast.db()).iter().rev() {
            let id = self.ast.variables().get_id(arg.id(self.ast.db()));
            let ty = self.ast.types().id_ty(id).into();
            prov_instr.push(Instruction::Pop(Address::Orig(id), ty));
        }

        let inner = TACBuilder::new_with_instructions(self.ast, prov_instr)
            .build_from_compound(f.compound(self.ast.db()));

        self.functions.insert(id, inner.1);
    }

    fn func_call(&mut self, f: FunctionCall) -> Option<Address> {
        let id = self.ast.variables().get_id(f.id(self.ast.db()));
        let mut out = Vec::new();
        for arg in f.args(self.ast.db()) {
            let expr = arg.expr(self.ast.db());
            let output_var = self.expr(expr);
            let expr_ty = self.ast.types().expr_ty(expr);
            out.push((output_var, expr_ty));
        }

        for (id, ty) in out {
            self.instructions
                .push(Instruction::Push(MemoryLocation::Address(id), ty.into()));
        }

        let func_ty = self.ast.types().func_ty(id);

        match func_ty.out {
            Some(ty) => {
                let out = self.next_temp();
                self.instructions
                    .push(Instruction::Call(id, out, ty.into()));
                Some(out)
            }
            None => {
                self.instructions.push(Instruction::CallNoRet(id));
                None
            }
        }
    }

    fn compound(&mut self, c: Compound) {
        for statement in c.statements(self.ast.db()) {
            match statement.downcast(self.ast.db()) {
                StatementType::Assign(a) => {
                    let id = self.ast.variables().get_id(a.id(self.ast.db()));
                    let id_ty = self.ast.types().id_ty(id).into();
                    let expr_output = MemoryLocation::Address(self.expr(a.expr(self.ast.db())));
                    self.instructions
                        .push(Instruction::Set(Address::Orig(id), expr_output, id_ty));
                }
                StatementType::Decl(d) => {
                    let expr = d.expr(self.ast.db());
                    if let Some(e) = expr {
                        let id = self.ast.variables().get_id(d.id(self.ast.db()));
                        let id_ty = self.ast.types().id_ty(id).into();
                        let expr_output = MemoryLocation::Address(self.expr(e));
                        self.instructions.push(Instruction::Set(
                            Address::Orig(id),
                            expr_output,
                            id_ty,
                        ));
                    }
                }
                StatementType::FunctionDecl(f) => {
                    self.declare_function(f);
                }
                StatementType::FunctionCall(f) => {
                    self.func_call(f);
                }
                StatementType::If(i) => {
                    let condition_var =
                        MemoryLocation::Address(self.expr(i.condition(self.ast.db())));
                    let end_label = self.next_label();
                    self.instructions.push(Instruction::ConditionalJump(
                        condition_var,
                        Type::Bool,
                        end_label,
                        false,
                    ));
                    self.compound(i.if_branch(self.ast.db()));

                    if let Some(else_branch) = i.else_branch(self.ast.db()) {
                        let else_label = self.next_label();
                        self.instructions.push(Instruction::Jump(else_label));
                        self.instructions.push(Instruction::Label(end_label));
                        self.compound(else_branch);
                        self.instructions.push(Instruction::Label(else_label));
                    } else {
                        self.instructions.push(Instruction::Label(end_label));
                    }
                }
                StatementType::While(w) => {
                    let start_label = self.next_label();
                    let end_label = self.next_label();
                    self.instructions.push(Instruction::Label(start_label));
                    let condition_var =
                        MemoryLocation::Address(self.expr(w.condition(self.ast.db())));
                    self.instructions.push(Instruction::ConditionalJump(
                        condition_var,
                        Type::Bool,
                        end_label,
                        false,
                    ));
                    self.compound(w.compound(self.ast.db()));
                    self.instructions.push(Instruction::Jump(start_label));
                    self.instructions.push(Instruction::Label(end_label));
                }
                StatementType::Return(r) => {
                    let expr = r.expr(self.ast.db());
                    let expr_output = MemoryLocation::Address(self.expr(expr));
                    let expr_ty = self.ast.types().expr_ty(expr).into();

                    self.instructions
                        .push(Instruction::Return(expr_output, expr_ty));
                }
                StatementType::PrintStat(p) => match p.downcast(self.ast.db()) {
                    PrintTypes::Get(g) => {
                        let id = self.ast.variables().get_id(g.id(self.ast.db()));
                        self.instructions.push(Instruction::CallBuiltin(
                            Builtin::Get,
                            Address::Orig(id),
                            VettedTy::Str,
                        ));
                    }
                    PrintTypes::Print(p) => {
                        let expr = p.expr(self.ast.db());
                        let expr_out = self.expr(expr);
                        let expr_ty = self.ast.types().expr_ty(expr).into();
                        self.instructions.push(Instruction::CallBuiltin(
                            Builtin::Print,
                            expr_out,
                            expr_ty,
                        ));
                    }
                    PrintTypes::Println(p) => {
                        let expr = p.expr(self.ast.db());
                        let expr_out = self.expr(expr);
                        let expr_ty = self.ast.types().expr_ty(expr).into();
                        self.instructions.push(Instruction::CallBuiltin(
                            Builtin::Println,
                            expr_out,
                            expr_ty,
                        ));
                    }
                },
            }
        }
    }

    fn expr(&mut self, e: Expr) -> Address {
        let head = e.head(self.ast.db());
        let tail = e.tail(self.ast.db());

        let head_ev = self.term(head);

        if let Some(tail) = tail {
            self.expr_prime(tail, head_ev)
        } else {
            head_ev
        }
    }

    fn expr_prime(&mut self, e: ExprPrime, lhs: Address) -> Address {
        let op = e.op(self.ast.db());
        let rhs = self.term(e.rhs(self.ast.db()));

        let out = self.next_temp();
        self.instructions
            .push(Instruction::Simple(SimpleInstruction {
                out,
                left: MemoryLocation::Address(lhs),
                right: MemoryLocation::Address(rhs),
                op: SimpleOp::Boolean(op),
                ty: VettedTy::Int,
            }));

        let tail = e.tail(self.ast.db());

        if let Some(tail) = tail {
            self.expr_prime(tail, out)
        } else {
            out
        }
    }

    fn term(&mut self, e: Term) -> Address {
        let head = e.head(self.ast.db());
        let tail = e.tail(self.ast.db());

        let head_ev = self.factor(head);

        if let Some(tail) = tail {
            self.term_prime(tail, head_ev)
        } else {
            head_ev
        }
    }

    fn term_prime(&mut self, e: TermPrime, lhs: Address) -> Address {
        let op = e.op(self.ast.db());
        let rhs = self.factor(e.rhs(self.ast.db()));

        let out = self.next_temp();
        self.instructions
            .push(Instruction::Simple(SimpleInstruction {
                out,
                left: MemoryLocation::Address(lhs),
                right: MemoryLocation::Address(rhs),
                op: SimpleOp::Relational(op),
                // Note that the two operands are ints
                ty: VettedTy::Int,
            }));

        let tail = e.tail(self.ast.db());

        if let Some(tail) = tail {
            self.term_prime(tail, out)
        } else {
            out
        }
    }

    fn factor(&mut self, e: Factor) -> Address {
        let head = e.head(self.ast.db());
        let tail = e.tail(self.ast.db());

        let head_ev = self.product(head);

        if let Some(tail) = tail {
            self.factor_prime(tail, head_ev)
        } else {
            head_ev
        }
    }

    fn factor_prime(&mut self, e: FactorPrime, lhs: Address) -> Address {
        let op = e.op(self.ast.db());
        let rhs = self.product(e.rhs(self.ast.db()));

        let out = self.next_temp();
        self.instructions
            .push(Instruction::Simple(SimpleInstruction {
                out,
                left: MemoryLocation::Address(lhs),
                right: MemoryLocation::Address(rhs),
                op: SimpleOp::Additive(op),
                ty: VettedTy::Int,
            }));

        let tail = e.tail(self.ast.db());

        if let Some(tail) = tail {
            self.factor_prime(tail, out)
        } else {
            out
        }
    }

    fn product(&mut self, e: Product) -> Address {
        let head = e.head(self.ast.db());
        let tail = e.tail(self.ast.db());

        let head_ev = self.atom(head);

        if let Some(tail) = tail {
            self.product_prime(tail, head_ev)
        } else {
            head_ev
        }
    }

    fn product_prime(&mut self, e: ProductPrime, lhs: Address) -> Address {
        let op = e.op(self.ast.db());
        let rhs = self.atom(e.rhs(self.ast.db()));

        let out = self.next_temp();
        self.instructions
            .push(Instruction::Simple(SimpleInstruction {
                out,
                left: MemoryLocation::Address(lhs),
                right: MemoryLocation::Address(rhs),
                op: SimpleOp::Multiplicative(op),
                ty: VettedTy::Int,
            }));

        let tail = e.tail(self.ast.db());

        if let Some(tail) = tail {
            self.product_prime(tail, out)
        } else {
            out
        }
    }

    fn atom(&mut self, a: Atom) -> Address {
        match a.downcast(self.ast.db()) {
            AtomType::Unit(u) => self.unit(u),
            AtomType::Not(a) => {
                let inner = self.atom(a);
                let out = self.next_temp();
                self.instructions
                    .push(Instruction::Simple(SimpleInstruction {
                        out,
                        left: MemoryLocation::Address(inner),
                        right: THROWAWAY,
                        op: SimpleOp::Not,
                        ty: VettedTy::Int,
                    }));
                out
            }
        }
    }

    fn unit(&mut self, u: Unit) -> Address {
        match u.downcast(self.ast.db()) {
            UnitType::Bool(b) => {
                let out = self.next_temp();
                self.instructions.push(Instruction::Set(
                    out,
                    MemoryLocation::Const(Const::Int(b as isize)),
                    VettedTy::Int,
                ));
                out
            }
            UnitType::Int(i) => {
                let out = self.next_temp();
                self.instructions.push(Instruction::Set(
                    out,
                    MemoryLocation::Const(Const::Int(i)),
                    VettedTy::Int,
                ));
                out
            }
            UnitType::Str(s) => {
                let out = self.next_temp();
                self.instructions.push(Instruction::Set(
                    out,
                    MemoryLocation::Const(Const::Str(s)),
                    VettedTy::Str,
                ));
                out
            }
            UnitType::BracketedExpr(e) => self.expr(e),
            UnitType::Identifier(i) => Address::Orig(self.ast.variables().get_id(i)),
            UnitType::FunctionCall(f) => self.func_call(f).expect("Type checking must have failed"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_display_snapshot;

    fn create_ast(input: &str) -> AST {
        use crate::lexer::lexicalize;
        use crate::lexer::scanner::scan;
        use crate::parser::parse;

        let node = parse(lexicalize(scan(input)));
        AST::new(node, input)
    }

    #[test]
    fn tac_simple() {
        let input = "program x begin var x := 0; end";
        let ast = create_ast(input);
        let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));
        assert_display_snapshot!(tac);
    }

    #[test]
    fn tac_complex_op() {
        let input = "program x begin var x := 1 + 2 + 3 * 4 + 5 < 6 / 9; end";
        let ast = create_ast(input);
        let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));
        assert_display_snapshot!(tac);
    }

    #[test]
    fn tac_func_def() {
        let input = r#"
        program mult
        begin

        procedure int times(var x ~ int, var y ~ int) begin
            return x * y;
        end

        var z := times(6, 9);
        end
        "#;
        let ast = create_ast(input);
        let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));
        assert_display_snapshot!(tac);
    }

    #[test]
    fn tac_if_else() {
        let input = r#"
        program mult
        begin

        var x;
        if (true) then
        begin
            x := 1;
        end else
        begin
            x := 2;
        end;

        end
        "#;
        let ast = create_ast(input);
        let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));
        assert_display_snapshot!(tac);
    }

    #[test]
    fn tac_if() {
        let input = r#"
        program mult
        begin

        var x := 1;
        if (true) then
        begin
            x := 2;
        end;

        end
        "#;
        let ast = create_ast(input);
        let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));
        assert_display_snapshot!(tac);
    }

    #[test]
    fn tac_while() {
        let input = r#"
        program mult
        begin

        var x := 10;
        var acc := 1;
        while (x > 0)
        begin
            acc := acc * x;
            x := x - 1;
        end;

        end
        "#;
        let ast = create_ast(input);
        let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));
        assert_display_snapshot!(tac);
    }

    #[test]
    fn tac_fibonacci() {
        let input = r#"
        program fib
        begin
            procedure int fib(var n ~ int)
            begin
                if (n == 0) then
                begin
                    return 0;
                end;

                if (n == 1) then
                begin
                    return 1;
                end;

                return fib(n - 1) + fib(n - 2);
            end
        end
        "#;
        let ast = create_ast(input);
        let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));
        assert_display_snapshot!(tac);
    }

}
