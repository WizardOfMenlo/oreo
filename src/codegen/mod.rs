//! Codegen generation

pub mod tac;
use crate::ast::IdentId;
use crate::ast::AST;
use tac::*;

use std::collections::HashMap;

enum Register {
    EAX,
}

enum ValueLocation {
    Register(Register),
    Stack(IdentId),
}

struct SimpleHLAInstr {
    left: ValueLocation,
    right: Register,
    op: SimpleOp,
}

enum HLAInstruction<'a> {
    Program(&'a str),
    BeginProgram(&'a str),
    EndProgram(&'a str),
    Procedure(usize),
    BeginProcedure(usize),
    EndProcedure(usize),
    MovFromMem(ValueLocation, Register),
    MovToMem(Register, ValueLocation),
    Simple(SimpleHLAInstr),
    Negate(ValueLocation),
}

struct HLABuilder<'a> {
    buf: Vec<HLAInstruction<'a>>,
    variables: HashMap<Address, ValueLocation>,
    stack: Vec<IdentId>,
}

impl<'a> HLABuilder<'a> {
    fn new() -> Self {
        HLABuilder {
            buf: Vec::new(),
            variables: HashMap::new(),
            stack: Vec::new(),
        }
    }

    fn build(mut self, global: &'a GlobalTAC) -> Vec<HLAInstruction<'a>> {
        // TODO: String definition
        self.buf.push(HLAInstruction::Program(&global.program_name));
        for (f_id, f_code) in &global.functions {
            self.buf.push(HLAInstruction::Procedure(f_id.0));
            self.buf.push(HLAInstruction::BeginProcedure(f_id.0));
            self.tac(f_code);
            self.buf.push(HLAInstruction::EndProcedure(f_id.0));
        }
        self.buf
            .push(HLAInstruction::BeginProgram(&global.program_name));
        self.tac(&global.global);
        self.buf
            .push(HLAInstruction::EndProgram(&global.program_name));
        self.buf
    }

    fn tac(&mut self, tac: &TAC) {}
}
