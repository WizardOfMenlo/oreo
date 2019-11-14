//! Codegen generation

pub mod tac;

use tac::*;

#[derive(Clone, Copy)]
enum AvailableRegister {
    Eax,
    Ecx,
}

impl AvailableRegister {
    fn iter() -> impl Iterator<Item = &'static AvailableRegister> {
        let values = &[AvailableRegister::Eax, AvailableRegister::Ecx];
        values.iter()
    }
}

struct Register {
    register: AvailableRegister,
    variable: Option<Address>,
}

impl Register {
    fn can_be_reused(&self) -> bool {
        match self.variable {
            Some(Address::Orig(_)) => false,
            _ => true,
        }
    }

    fn assign_to_address(&mut self, a: Address) {
        self.variable = Some(a);
    }
}

struct HLATranslator {
    buf: String,
    registers: Vec<Register>,
    stack: Vec<Address>,
}

impl HLATranslator {
    fn new() -> Self {
        HLATranslator {
            buf: String::new(),
            registers: AvailableRegister::iter()
                .map(|r| Register {
                    register: *r,
                    variable: None,
                })
                .collect(),
            stack: Vec::new(),
        }
    }

    fn build(self, t: GlobalTAC) -> String {
        self.buf
    }
}
