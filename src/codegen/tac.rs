//! Module with facilities for Three Address Code gen

use crate::ast::AST;
use crate::ast::node_db::NodeId;
use crate::ast::IdentId;
use crate::common;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct Label(usize);

enum Const {
    Int(usize),
    Str,
    Bool(bool),
}


enum SimpleOp {
    Multiplicative(common::MultiplicativeOp),
    Additive(common::AdditiveOp),
    Boolean(common::BooleanOp),
    Relational(common::RelationalOp),
}

enum MemoryLocation {
    Address(Address),
    Const(Const)
}

enum Address {
    Temp(usize),
    Orig(IdentId),
}



enum Instruction {
    Jump(Label),
    ConditionalJump(MemoryLocation, Label),
    Simple(SimpleInstruction),
    Set(Address, MemoryLocation),
    Push(MemoryLocation),
    Pop(Address),
}

struct SimpleInstruction {
    out: Address,
    left: MemoryLocation,
    right: MemoryLocation,
    op: SimpleOp,
}


struct TAC {
    labels: HashMap<Label, usize>,
    instructions: Vec<Instruction>,
}

struct TACBuilder<'a, 'b> {
    ast: &'b AST<'a>,
    current_id: usize,
    current_label: usize,
    labels: HashMap<Label, usize>,
    instructions: Vec<Instruction>
}


use crate::ast::syntax::*;
impl<'a, 'b> TACBuilder<'a, 'b> {
    fn new(ast : &'b AST<'a>) -> Self {
        TACBuilder {
            ast,
            current_id: 1,
            current_label: 0,
            labels: HashMap::new(),
            instructions: Vec::new(),
        }
    }


    fn build(mut self, p: Program) -> TAC {
        self.compound(p.compound(self.ast.db()));

        TAC { labels: self.labels, instructions: self.instructions }
    }

    fn compound(&self, c: Compound)  {
        for statement in c.statements(self.ast.db()) {
            /*
            match statement.downcast(self.ast.db()) {
                StatementType::Assign(a) => {}
                StatementType::Decl(d) => {}
            }
            */
        } 
    }

}