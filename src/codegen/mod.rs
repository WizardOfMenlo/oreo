//! Codegen generation

pub mod tac;
use crate::ast::IdentId;
use crate::ast::NodeId;
use crate::ast::AST;
use tac::*;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Register {
    EAX,
    ECX,
    EBX,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueLocation {
    Register(Register),
    Stack(IdentId),
}

#[derive(Debug)]
pub struct SimpleHLAInstr {
    left: ValueLocation,
    right: Register,
    op: SimpleOp,
}

#[derive(Debug)]
pub enum HLAInstruction<'a> {
    Program(&'a str),
    BeginProgram(&'a str),
    EndProgram(&'a str),
    Procedure(usize),
    BeginProcedure(usize),
    EndProcedure(usize),
    DeclInt(ValueLocation),
    DeclStr(ValueLocation),
    MovFromMem(ValueLocation, Register),
    MovToMem(Register, ValueLocation),
    SetInt(Register, isize),
    SetStr(Register, NodeId),
    Simple(SimpleHLAInstr),
    Negate(ValueLocation),
    OutputStr(ValueLocation),
    OutputInt(ValueLocation),
    SetComp(Register),
    Label(Label),
    Jump(Label),
    CondJump(Label, bool),
}

#[derive(Debug)]
pub struct HLABuilder<'a> {
    buf: Vec<HLAInstruction<'a>>,
    variables: HashMap<Address, ValueLocation>,
    free_registers: Vec<Register>,
}

impl<'a> HLABuilder<'a> {
    pub fn new() -> Self {
        HLABuilder {
            buf: Vec::new(),
            variables: HashMap::new(),
            // Note, I think there is a corner case with one register
            free_registers: vec![Register::EAX, Register::EBX, Register::ECX],
        }
    }

    pub fn build(mut self, global: &'a GlobalTAC) -> Vec<HLAInstruction<'a>> {
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

    fn tac(&mut self, tac: &TAC) {
        for (var, ty) in &tac.stack {
            self.buf.push(match ty {
                VettedTy::Int => HLAInstruction::DeclInt(ValueLocation::Stack(*var)),
                VettedTy::Str => HLAInstruction::DeclStr(ValueLocation::Stack(*var)),
            })
        }

        for instruction in &tac.instructions {
            println!("{}", instruction);
            match instruction {
                Instruction::Jump(l) => self.buf.push(HLAInstruction::Jump(*l)),
                Instruction::Label(l) => self.buf.push(HLAInstruction::Label(*l)),
                Instruction::Set(addr, mem, _) => {
                    // Get a valid register (Note, we need to protect mem if temp)
                    let reg = self.first_available_register();
                    match mem {
                        // If we are setting from somewhere
                        MemoryLocation::Address(input) => {
                            let loc = self.variables.get(input).unwrap();
                            self.buf.push(HLAInstruction::MovFromMem(*loc, reg));
                        }
                        // If we are just loading a constant
                        MemoryLocation::Const(c) => match c {
                            Const::Int(i) => self.buf.push(HLAInstruction::SetInt(reg, *i)),
                            Const::Str(s) => self.buf.push(HLAInstruction::SetStr(reg, *s)),
                        },
                    }

                    // Set that this variable will go to this register
                    self.variables.insert(*addr, ValueLocation::Register(reg));
                }
                Instruction::ConditionalJump(mem, _, label, b) => {
                    match mem {
                        // If we are doing it with a const, just optimize it
                        MemoryLocation::Const(c) => match c {
                            Const::Int(i) => {
                                if *i == 0 && *b || *i != 0 && !b {
                                    self.buf.push(HLAInstruction::Jump(*label))
                                }
                            }
                            Const::Str(_) => panic!("Cannot cmp str"),
                        },
                        // Else load from mem and then set comparision
                        MemoryLocation::Address(addr) => {
                            let reg = self.load_to_register(*addr);
                            self.buf.push(HLAInstruction::SetComp(reg));
                            self.buf.push(HLAInstruction::CondJump(*label, *b))
                        }
                    }
                }
                Instruction::Not(out, mem, _) => {
                    let reg = match mem {
                        MemoryLocation::Const(c) => match c {
                            Const::Int(i) => {
                                let free = self.first_available_register();
                                self.buf.push(HLAInstruction::SetInt(free, *i));
                                free
                            }
                            Const::Str(_) => panic!("Cannot negate str"),
                        },
                        MemoryLocation::Address(addr) => self.load_to_register(*addr),
                    };

                    let loc = ValueLocation::Register(reg);
                    self.buf.push(HLAInstruction::Negate(loc));
                    self.variables.insert(*out, loc);
                }
                Instruction::Simple(instr) => {}
                _ => panic!("Not implemented yet"),
            };
        }
    }

    fn load_to_register(&mut self, addr: Address) -> Register {
        // Find where the thing is stored
        let curr_loc = *self.variables.get(&addr).unwrap();
        match curr_loc {
            // If it is already in a reg
            ValueLocation::Register(r) => r,
            // Else load to the first available one
            ValueLocation::Stack(_) => {
                let next = self.first_available_register();
                self.buf.push(HLAInstruction::MovFromMem(curr_loc, next));
                self.variables.insert(addr, ValueLocation::Register(next));
                next
            }
        }
    }

    fn first_available_register(&mut self) -> Register {
        // If we have something, give it
        if !self.free_registers.is_empty() {
            return self.free_registers.pop().unwrap();
        }

        // If we are here we need to free up some space
        let mut vars = self.vars_in_register();
        use std::cmp::Ordering;
        vars.sort_by(|(f, _), (s, _)| match (f, s) {
            (Address::Orig(_), Address::Temp(_)) => Ordering::Greater,
            (Address::Temp(_), Address::Orig(_)) => Ordering::Less,
            (Address::Temp(f), Address::Temp(s)) => f.cmp(&s),
            (Address::Orig(f), Address::Orig(s)) => f.0.cmp(&s.0),
        });

        let one_to_evict = vars.get(0).unwrap();

        match one_to_evict.0 {
            // We can always safely remove temp variables
            Address::Temp(_) => {
                self.variables.remove(&one_to_evict.0);
            }
            Address::Orig(ident) => {
                let loc = ValueLocation::Stack(ident);
                // Store to mem
                self.buf.push(HLAInstruction::MovToMem(one_to_evict.1, loc));
                self.variables.insert(one_to_evict.0, loc);
            }
        }

        one_to_evict.1
    }

    fn vars_in_register(&self) -> Vec<(Address, Register)> {
        self.variables
            .iter()
            .flat_map(|(a, l)| match l {
                ValueLocation::Register(r) => Some((*a, *r)),
                _ => None,
            })
            .collect()
    }
}
