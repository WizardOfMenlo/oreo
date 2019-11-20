//! Codegen generation

pub mod tac;
use crate::ast::IdentId;
use crate::ast::NodeId;
use crate::ast::AST;
use crate::common;
use std::fmt;
use tac::*;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Register {
    EAX,
    ECX,
    EBX,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::EAX => write!(f, "eax"),
            Register::EBX => write!(f, "ebx"),
            Register::ECX => write!(f, "ecx"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueLocation {
    Register(Register),
    Stack(IdentId),
}

impl fmt::Display for ValueLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueLocation::Register(r) => write!(f, "{}", r),
            ValueLocation::Stack(id) => write!(f, "o{}", id.0),
        }
    }
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
    Procedure(usize, Vec<(IdentId, VettedTy)>),
    BeginProcedure(usize),
    EndProcedure(usize),
    DeclInt(IdentId),
    DeclStr(IdentId),
    MovFromMem(ValueLocation, Register),
    MovToMem(Register, ValueLocation),
    SetInt(Register, isize),
    SetStr(Register, NodeId),
    Simple(SimpleHLAInstr),
    Negate(ValueLocation),
    OutputStr(ValueLocation),
    OutputInt(ValueLocation),
    GetStr(ValueLocation),
    SetComp(Register),
    Label(Label),
    Jump(Label),
    CondJump(Label, bool),
    Call(IdentId, Vec<ValueLocation>),
    Return(Register),
    Static,
}

fn write_decl_args(args: &[(IdentId, VettedTy)]) -> String {
    let mut buf = String::new();
    for (arg, ty) in args {
        buf.push_str(&format!(
            "o{} : {},",
            arg.0,
            match ty {
                VettedTy::Int => "i32",
                VettedTy::Str => "str",
            }
        ));
    }

    buf
}

fn write_call_args(args: &[ValueLocation]) -> String {
    let mut buf = String::new();
    for loc in args {
        buf.push_str(&format!("{},", loc));
    }

    buf
}

impl<'a> fmt::Display for HLAInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HLAInstruction::Program(p) => write!(f, "program {};", p),
            HLAInstruction::BeginProgram(p) => write!(f, "begin {};", p),
            HLAInstruction::EndProgram(p) => write!(f, "end {};", p),
            HLAInstruction::Procedure(id, args) => write!(
                f,
                "procedure f{} ({}); @return(eax)",
                id,
                write_decl_args(args)
            ),
            HLAInstruction::BeginProcedure(id) => write!(f, "begin f{};", id),
            HLAInstruction::EndProcedure(id) => write!(f, "end f{};", id),
            HLAInstruction::Static => write!(f, "static:"),
            HLAInstruction::DeclInt(id) => write!(f, "o{} : i32;", id.0),
            HLAInstruction::DeclStr(id) => write!(f, "o{} : str;", id.0),
            HLAInstruction::MovFromMem(mem, reg) => write!(f, "mov({}, {});", mem, reg),
            HLAInstruction::MovToMem(reg, mem) => write!(f, "mov({}, {});", reg, mem),
            HLAInstruction::SetInt(reg, i) => write!(f, "mov({}, {});", i, reg),
            HLAInstruction::SetStr(reg, i) => write!(f, "mov(s{}, {});", i.0, reg),
            HLAInstruction::Negate(loc) => write!(f, "not({});", loc),
            HLAInstruction::OutputStr(loc) => write!(f, "stdout.puts({});", loc),
            HLAInstruction::OutputInt(loc) => write!(f, "stdout.puti({});", loc),
            HLAInstruction::GetStr(loc) => write!(f, "stdin.a_gets({});", loc),
            HLAInstruction::SetComp(r) => write!(f, "set({});", r),
            HLAInstruction::Label(l) => write!(f, "label {};", l),
            HLAInstruction::Jump(l) => write!(f, "jmp {};", l),
            HLAInstruction::CondJump(l, b) => {
                if *b {
                    write!(f, "je {};", l)
                } else {
                    write!(f, "jne {};", l)
                }
            }
            HLAInstruction::Call(id, args) => write!(f, "f{}({});", id.0, write_call_args(args)),

            HLAInstruction::Return(r) => write!(f, "@return ({});", r),
            HLAInstruction::Simple(s) => {
                let op = s.op;
                match op {
                    SimpleOp::Additive(add) => match add {
                        common::AdditiveOp::Plus => write!(f, "add({}, {});", s.left, s.right),
                        common::AdditiveOp::Minus => write!(f, "sub({}, {});", s.left, s.right),
                    },
                    SimpleOp::Multiplicative(mult) => match mult {
                        common::MultiplicativeOp::Times => {
                            write!(f, "mult({}, {});", s.left, s.right)
                        }
                        common::MultiplicativeOp::Divide => {
                            write!(f, "div({}, {});", s.left, s.right)
                        }
                    },
                    SimpleOp::Boolean(boo) => match boo {
                        common::BooleanOp::And => write!(f, "and({}, {});", s.left, s.right),
                        common::BooleanOp::Or => write!(f, "or({}, {});", s.left, s.right),
                    },
                    SimpleOp::Relational(boo) => match boo {
                        common::RelationalOp::Equals => write!(f, "eq({}, {});", s.left, s.right),
                        common::RelationalOp::LesserOrEquals => {
                            write!(f, "leq({}, {});", s.left, s.right)
                        }
                        common::RelationalOp::LesserThan => {
                            write!(f, "lt({}, {});", s.left, s.right)
                        }
                        common::RelationalOp::GreaterOrEquals => {
                            write!(f, "geq({}, {});", s.left, s.right)
                        }
                        common::RelationalOp::GreaterThan => {
                            write!(f, "gt({}, {});", s.left, s.right)
                        }
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct HLABuilder<'a> {
    buf: Vec<HLAInstruction<'a>>,
    variables: HashMap<IdentId, ValueLocation>,
    temps: HashMap<usize, Register>,
    free_registers: Vec<Register>,
}

impl<'a> HLABuilder<'a> {
    pub fn new(registers: Vec<Register>) -> Self {
        HLABuilder {
            buf: Vec::new(),
            variables: HashMap::new(),
            temps: HashMap::new(),
            free_registers: registers,
        }
    }

    /// Get the default regs
    pub fn default_regs() -> Vec<Register> {
        vec![Register::EAX, Register::EBX, Register::ECX]
    }

    pub fn build(mut self, global: &'a GlobalTAC) -> Vec<HLAInstruction<'a>> {
        // TODO: String definition
        self.buf.push(HLAInstruction::Program(&global.program_name));
        for (f_id, (f_code, args)) in &global.functions {
            self.buf
                .push(HLAInstruction::Procedure(f_id.0, args.clone()));
            self.add_variables(&f_code.stack);
            self.buf.push(HLAInstruction::BeginProcedure(f_id.0));
            self.tac(f_code);
            self.buf.push(HLAInstruction::EndProcedure(f_id.0));
        }

        self.add_variables(&global.global.stack);
        self.buf
            .push(HLAInstruction::BeginProgram(&global.program_name));
        self.tac(&global.global);
        self.buf
            .push(HLAInstruction::EndProgram(&global.program_name));
        self.buf
    }

    // Add all the declaration variables
    fn add_variables(&mut self, vars: &HashMap<IdentId, VettedTy>) {
        self.buf.push(HLAInstruction::Static);
        for (var, ty) in vars {
            self.buf.push(match ty {
                VettedTy::Int => HLAInstruction::DeclInt(*var),
                VettedTy::Str => HLAInstruction::DeclStr(*var),
            })
        }
    }

    fn tac(&mut self, tac: &TAC) {
        for instruction in &tac.instructions {
            match instruction {
                Instruction::Jump(l) => self.buf.push(HLAInstruction::Jump(*l)),
                Instruction::Label(l) => self.buf.push(HLAInstruction::Label(*l)),
                Instruction::Set(addr, mem, _) => {
                    self.load_memory_location(*addr, *mem);
                }
                Instruction::ConditionalJump(mem, _, label, b) => {
                    match mem {
                        // Optimize constants
                        MemoryLocation::Const(c) => match c {
                            Const::Int(i) => {
                                if *b && *i != 0 || !b && *i == 0 {
                                    self.buf.push(HLAInstruction::Jump(*label));
                                }
                            }
                            Const::Str(_) => panic!("Invalid TAC"),
                        },
                        MemoryLocation::Address(addr) => {
                            let reg = self.load_into_register(*addr);
                            self.buf.push(HLAInstruction::SetComp(reg));
                            self.buf.push(HLAInstruction::CondJump(*label, *b));
                        }
                    }
                }
                Instruction::Not(out, mem, _) => {
                    let out = self.load_memory_location(*out, *mem);
                    self.buf
                        .push(HLAInstruction::Negate(ValueLocation::Register(out)));
                }
                Instruction::Simple(instr) => {
                    let right =
                        self.load_memory_location(instr.out, MemoryLocation::Address(instr.right));
                    let left = self.load_into_register(instr.left);

                    self.buf.push(HLAInstruction::Simple(SimpleHLAInstr {
                        right,
                        left: ValueLocation::Register(left),
                        op: instr.op,
                    }));
                }
                Instruction::CallBuiltin(built, addr, ty) => {
                    let location = self.get_location(*addr);
                    match ty {
                        VettedTy::Int => match built {
                            Builtin::Get => panic!("Cant get int"),
                            Builtin::Print | Builtin::Println => {
                                self.buf.push(HLAInstruction::OutputInt(location))
                            }
                        },
                        VettedTy::Str => match built {
                            Builtin::Get => self.buf.push(HLAInstruction::GetStr(location)),
                            Builtin::Print | Builtin::Println => {
                                self.buf.push(HLAInstruction::OutputStr(location))
                            }
                        },
                    }
                }
                Instruction::Call(id, ret, _, args) => {
                    self.free_register(Register::EAX);
                    let args = args.iter().map(|i| self.get_location(*i)).collect();
                    self.buf.push(HLAInstruction::Call(*id, args));
                    self.point_addr_to_register(*ret, Register::EAX);
                }
                Instruction::CallNoRet(id, args) => {
                    let args = args.iter().map(|i| self.get_location(*i)).collect();
                    self.buf.push(HLAInstruction::Call(*id, args));
                }
                Instruction::Return(mem, _) => {
                    self.free_register(Register::EAX);
                    self.set_mem_to_reg(*mem, Register::EAX);
                    self.buf.push(HLAInstruction::Return(Register::EAX));
                }
            };
        }
    }

    // Save an identifier
    fn write_to_mem(&mut self, id: IdentId) {
        let loc = *self.variables.get(&id).unwrap();
        match loc {
            ValueLocation::Stack(_) => {}
            ValueLocation::Register(r) => {
                self.free_registers.push(r);
                let new_loc = ValueLocation::Stack(id);
                self.variables.insert(id, new_loc);
                self.buf.push(HLAInstruction::MovToMem(r, new_loc));
            }
        }
    }

    // Take ownership of a temporary to put into a identifier
    fn rename(&mut self, temp: usize, res: Address) -> Register {
        let temp_reg = self.get_temp_register(temp);

        match res {
            Address::Orig(id) => {
                self.variables.insert(id, ValueLocation::Register(temp_reg));
            }
            Address::Temp(t) => {
                self.temps.insert(t, temp_reg);
            }
        };

        temp_reg
    }

    fn copy_to_new_dest(&mut self, id: IdentId, dest: Address) -> Register {
        let dest = self.init_address(dest);
        let old_lco = self.variables.get(&id).unwrap();
        self.buf.push(HLAInstruction::MovFromMem(*old_lco, dest));
        dest
    }

    // Reading destructs the register
    fn get_temp_register(&mut self, temp: usize) -> Register {
        let register = *self.temps.get(&temp).unwrap();
        self.temps.remove(&temp);
        register
    }

    // Get a register initted to point to the address
    fn init_address(&mut self, addr: Address) -> Register {
        let r = self.next_available_register();
        match addr {
            Address::Orig(id) => {
                self.variables.insert(id, ValueLocation::Register(r));
            }
            Address::Temp(t) => {
                self.temps.insert(t, r);
            }
        };

        r
    }

    fn set_const(&mut self, addr: Address, c: Const) -> Register {
        let reg = self.init_address(addr);
        self.buf.push(match c {
            Const::Int(i) => HLAInstruction::SetInt(reg, i),
            Const::Str(i) => HLAInstruction::SetStr(reg, i),
        });

        reg
    }

    fn get_location(&mut self, addr: Address) -> ValueLocation {
        match addr {
            Address::Orig(id) => *self.variables.get(&id).unwrap(),
            Address::Temp(i) => ValueLocation::Register(*self.temps.get(&i).unwrap()),
        }
    }

    fn load_into_register(&mut self, addr: Address) -> Register {
        match addr {
            Address::Temp(i) => self.get_temp_register(i),
            Address::Orig(i) => self.copy_to_new_dest(i, addr),
        }
    }

    fn load_memory_location(&mut self, addr: Address, mem: MemoryLocation) -> Register {
        match mem {
            MemoryLocation::Address(a) => match a {
                Address::Orig(orig) => self.copy_to_new_dest(orig, addr),
                Address::Temp(t) => self.rename(t, addr),
            },
            MemoryLocation::Const(c) => self.set_const(addr, c),
        }
    }

    fn set_mem_to_reg(&mut self, mem: MemoryLocation, reg: Register) {
        match mem {
            MemoryLocation::Address(a) => match a {
                Address::Orig(orig) => {
                    let old_lco = self.variables.get(&orig).unwrap();
                    self.buf.push(HLAInstruction::MovFromMem(*old_lco, reg));
                }
                Address::Temp(t) => {
                    let old_reg = self.get_temp_register(t);
                    self.buf.push(HLAInstruction::MovFromMem(
                        ValueLocation::Register(old_reg),
                        reg,
                    ));
                }
            },
            MemoryLocation::Const(c) => self.buf.push(match c {
                Const::Int(i) => HLAInstruction::SetInt(reg, i),
                Const::Str(i) => HLAInstruction::SetStr(reg, i),
            }),
        }
    }

    fn point_addr_to_register(&mut self, addr: Address, r: Register) {
        match addr {
            Address::Orig(orig) => {
                self.variables.insert(orig, ValueLocation::Register(r));
            }
            Address::Temp(i) => {
                self.temps.insert(i, r);
            }
        };
    }

    fn free_register(&mut self, reg: Register) {
        if self.free_registers.contains(&reg) {
            return;
        }

        let temp = self.temps.iter().find(|(_, &r)| r == reg).map(|(t, _)| *t);
        if let Some(temp) = temp {
            let new_reg = self.next_available_register();
            self.buf.push(HLAInstruction::MovFromMem(
                ValueLocation::Register(reg),
                new_reg,
            ));
            self.temps.insert(temp, new_reg);
        }

        let orig = self
            .variables
            .iter()
            .find(|(_, loc)| match loc {
                ValueLocation::Register(r) if *r == reg => true,
                _ => false,
            })
            .map(|(i, _)| *i);

        if let Some(id) = orig {
            self.write_to_mem(id);
        }
    }

    fn next_available_register(&mut self) -> Register {
        if !self.free_registers.is_empty() {
            return self.free_registers.pop().unwrap();
        }

        let first_glob_in_reg = self
            .variables
            .iter()
            .flat_map(|(i, loc)| match loc {
                ValueLocation::Register(r) => Some((*i, *r)),
                _ => None,
            })
            .next()
            .expect("Could not solve allocation problem");

        self.write_to_mem(first_glob_in_reg.0);

        first_glob_in_reg.1
    }
}
