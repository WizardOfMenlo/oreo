//! Module with facilities for Three Address Code gen

use crate::ast::IdentId;
use crate::ast::NodeId;
use crate::ast::AST;
use crate::common;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct Label(usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
enum Const {
    Int(isize),
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

#[derive(Debug, Clone, Copy)]
enum SimpleOp {
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

#[derive(Debug, Copy, Clone)]
enum MemoryLocation {
    Address(Address),
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

const THROWAWAY: MemoryLocation = MemoryLocation::Address(Address::Temp(0));

#[derive(Debug, Copy, Clone)]
enum Address {
    Temp(usize),
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

#[derive(Debug, Copy, Clone)]
enum Builtin {
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

#[derive(Debug, Clone)]
enum Instruction {
    Jump(Label),
    Return(MemoryLocation),
    ConditionalJump(MemoryLocation, Label, bool),
    Simple(SimpleInstruction),
    Set(Address, MemoryLocation),
    Push(MemoryLocation),
    Pop(Address),
    Call(IdentId),
    CallBuiltin(Builtin, Address),
    Label(Label),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Jump(l) => write!(f, "jmp {}", l),
            Instruction::Return(m) => write!(f, "Return {}", m),
            Instruction::ConditionalJump(m, l, b) => {
                write!(f, "jmp {} {}{}", l, if *b { "" } else { "!" }, m)
            }
            Instruction::Set(a, m) => write!(f, "{} := {}", a, m),
            Instruction::Push(m) => write!(f, "Push {}", m),
            Instruction::Pop(a) => write!(f, "Pop {}", a),
            Instruction::Call(i) => write!(f, "Call f{}", i.0),
            Instruction::CallBuiltin(b, a) => write!(f, "Call {} {}", b, a),
            Instruction::Simple(simple) => write!(f, "{}", simple),
            Instruction::Label(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Debug, Clone)]
struct SimpleInstruction {
    out: Address,
    left: MemoryLocation,
    right: MemoryLocation,
    op: SimpleOp,
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

#[derive(Debug)]
struct GlobalTAC {
    functions: HashMap<IdentId, TAC>,
    global: TAC,
}

impl fmt::Display for GlobalTAC {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (func_id, code) in &self.functions {
            writeln!(f, "BeginFunc f{}", func_id.0)?;
            write!(f, "{}", code)?;
            writeln!(f, "EndFunc")?;
        }

        write!(f, "{}", self.global)?;

        Ok(())
    }
}

#[derive(Debug)]
struct TAC {
    instructions: Vec<Instruction>,
}

impl fmt::Display for TAC {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
struct TACBuilder<'a, 'b> {
    ast: &'b AST<'a>,
    current_id: usize,
    current_label: usize,
    instructions: Vec<Instruction>,
    functions: HashMap<IdentId, TAC>,
}

use crate::ast::syntax::*;
impl<'a, 'b> TACBuilder<'a, 'b> {
    fn new(ast: &'b AST<'a>) -> Self {
        Self::new_with_instructions(ast, Vec::new())
    }

    fn new_with_instructions(ast: &'b AST<'a>, instructions: Vec<Instruction>) -> Self {
        TACBuilder {
            ast,
            current_id: 1,
            current_label: 0,
            functions: HashMap::new(),
            instructions,
        }
    }

    fn build(self, p: Program) -> GlobalTAC {
        let comp = p.compound(self.ast.db());
        let global = self.build_from_compound(comp);
        GlobalTAC {
            functions: global.0.functions,
            global: global.1,
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
        for arg in f.args(self.ast.db()) {
            let id = self.ast.variables().get_id(arg.id(self.ast.db()));
            prov_instr.push(Instruction::Pop(Address::Orig(id)));
        }

        let inner = TACBuilder::new_with_instructions(self.ast, prov_instr)
            .build_from_compound(f.compound(self.ast.db()));

        self.functions.insert(id, inner.1);
    }

    fn func_call(&mut self, f: FunctionCall) -> Address {
        let id = self.ast.variables().get_id(f.id(self.ast.db()));
        let mut out = Vec::new();
        for arg in f.args(self.ast.db()) {
            out.push(self.expr(arg.expr(self.ast.db())));
        }

        for id in out {
            self.instructions
                .push(Instruction::Push(MemoryLocation::Address(id)));
        }

        self.instructions.push(Instruction::Call(id));

        let out = self.next_temp();

        self.instructions.push(Instruction::Pop(out));

        out
    }

    fn compound(&mut self, c: Compound) {
        for statement in c.statements(self.ast.db()) {
            match statement.downcast(self.ast.db()) {
                StatementType::Assign(a) => {
                    let id = self.ast.variables().get_id(a.id(self.ast.db()));
                    let expr_output = MemoryLocation::Address(self.expr(a.expr(self.ast.db())));
                    self.instructions
                        .push(Instruction::Set(Address::Orig(id), expr_output));
                }
                StatementType::Decl(d) => {
                    let expr = d.expr(self.ast.db());
                    if let Some(e) = expr {
                        let id = self.ast.variables().get_id(d.id(self.ast.db()));
                        let expr_output = MemoryLocation::Address(self.expr(e));
                        self.instructions
                            .push(Instruction::Set(Address::Orig(id), expr_output));
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
                        end_label,
                        false,
                    ));
                    self.compound(w.compound(self.ast.db()));
                    self.instructions.push(Instruction::Jump(start_label));
                    self.instructions.push(Instruction::Label(end_label));
                }
                StatementType::Return(r) => {
                    let expr = MemoryLocation::Address(self.expr(r.expr(self.ast.db())));
                    self.instructions.push(Instruction::Return(expr));
                }
                StatementType::PrintStat(p) => match p.downcast(self.ast.db()) {
                    PrintTypes::Get(g) => {
                        let id = self.ast.variables().get_id(g.id(self.ast.db()));
                        self.instructions
                            .push(Instruction::CallBuiltin(Builtin::Get, Address::Orig(id)));
                    }
                    PrintTypes::Print(p) => {
                        let expr = self.expr(p.expr(self.ast.db()));
                        self.instructions
                            .push(Instruction::CallBuiltin(Builtin::Print, expr));
                    }
                    PrintTypes::Println(p) => {
                        let expr = self.expr(p.expr(self.ast.db()));
                        self.instructions
                            .push(Instruction::CallBuiltin(Builtin::Println, expr));
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
                ));
                out
            }
            UnitType::Int(i) => {
                let out = self.next_temp();
                self.instructions
                    .push(Instruction::Set(out, MemoryLocation::Const(Const::Int(i))));
                out
            }
            UnitType::Str(s) => {
                let out = self.next_temp();
                self.instructions
                    .push(Instruction::Set(out, MemoryLocation::Const(Const::Str(s))));
                out
            }
            UnitType::BracketedExpr(e) => self.expr(e),
            UnitType::Identifier(i) => Address::Orig(self.ast.variables().get_id(i)),
            UnitType::FunctionCall(f) => self.func_call(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_display_snapshot;

    fn create_ast<'a>(input: &'a str) -> AST<'a> {
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
}
