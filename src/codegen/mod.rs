//! Codegen generation

pub mod tac;
use crate::ast::AST;
use tac::*;

use std::collections::HashMap;

fn hla(g: GlobalTAC, ast: &AST<'_>) {
    println!("program {};", &g.program_name);
    for (f_id, f_code) in g.functions {
        println!("procedure f{}", f_id.0);
        println!("begin f{}", f_id.0);
        tac(f_code, ast);
        println!("end f{}", f_id.0);
    }
    println!("begin {};", &g.program_name);
    tac(g.global, ast);
    println!("end {};", &g.program_name);
}

fn tac(g: TAC, ast: &AST<'_>) {
    for instr in g.instructions {
        /*
        match instr {
            Instruction::Jump(l) => println!("jmp {};", l),
            Instruction::Label(l) => println!("{}:", l),
        }
        */
    }
}

enum Register {
    EAX,
}

enum ValueLocation {
    Register(Register),
    Stack(usize),
}

struct HLABuilder<'a, 'b> {
    ast: &'b AST<'a>,
    buf: String,
    variables: HashMap<Address, ValueLocation>,
    stack: Vec<Address>,
}