use super::bytecode::*;
use super::literal::*;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};

use log::*;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Registers {
    global: Vec<Value>,
    local: VecDeque<Vec<Value>>,
    current_local: Vec<Value>,
}

impl Default for Registers {
    fn default() -> Registers {
        Registers::new()
    }
}

impl Registers {
    fn empty_register() -> Vec<Value> {
        [0; 4].to_vec().iter().map(|_| Value::Nil).collect()
    }

    pub fn new() -> Registers {
        let empty = Registers::empty_register();
        Registers {
            global: empty.clone(),
            local: VecDeque::new(),
            current_local: empty,
        }
    }

    pub fn global(&self) -> Vec<Value> {
        self.global.clone()
    }

    pub fn local(&self) -> Vec<Value> {
        self.current_local.clone()
    }

    pub fn shift_locals(&mut self, amount: u8) -> &mut Registers {
        self.current_local = self.current_local.as_slice()[amount as usize..].to_vec();
        self
    }

    pub fn push_new_local(&mut self) -> &mut Registers {
        self.local.push_back(self.current_local.clone());
        self.current_local = Registers::empty_register();
        self
    }

    pub fn restore_last_local(&mut self) -> &mut Registers {
        match self.local.pop_back() {
            None => self.current_local = Registers::empty_register(),
            Some(regs) => self.current_local = regs,
        }
        self
    }

    pub fn clear(&mut self, r: &Register) -> &mut Registers {
        self.put(&r, Value::Nil);
        self
    }

    pub fn clear_globals(&mut self, n: u32) -> &mut Registers {
        for i in (n as usize)..self.global.len() {
            self.global[i] = Value::Nil;
        }
        self
    }

    pub fn fill_globals_from_offset(&mut self, offset: u32, vs: &[Literal]) -> &mut Registers {
        for (i, v) in vs.iter().enumerate() {
            self.global[offset as usize + i] = v.clone().into();
        }
        self
    }

    pub fn put_global(&mut self, n: u32, v: Value) -> &mut Registers {
        self.global[n as usize] = v;
        self
    }

    pub fn put(&mut self, r: &Register, v: Value) -> &mut Registers {
        match r {
            Register::Local(l) => self.current_local[*l as usize] = v,
            Register::Global(g) => self.global[*g as usize] = v,
        }
        self
    }

    pub fn get(&self, r: &Register) -> Value {
        trace!("fetching register: {}", r);
        match r {
            Register::Local(l) => self.current_local[*l as usize].clone(),
            Register::Global(g) => self.global[*g as usize].clone(),
        }
    }

    pub fn get_many_literals_from_global_range(&self, from: u32, to: u32) -> Vec<Literal> {
        trace!("fetching global values from {} to {}", from, to);
        self.global[from as usize..to as usize]
            .iter()
            .map(|v| self.get_literal_from_value(v))
            .collect()
    }

    pub fn get_literal_from_value(&self, v: &Value) -> Literal {
        trace!("fetching value: {}", v);
        match v {
            Value::Literal(l) => l.clone(),
            Value::Register(reg) => self.get_literal_from_value(&self.get(reg)),
            Value::Nil => Literal::List(List::Nil),
        }
    }

    pub fn copy(&mut self, a: &Register, b: &Register) -> &mut Registers {
        trace!("copy value from {} to {}", a, b);
        self.put(b, self.get(&a));
        self
    }

    pub fn swap(&mut self, a: Register, b: Register) {
        match (a, b) {
            (Register::Local(l0), Register::Local(l1)) => {
                self.current_local.swap(l0 as usize, l1 as usize);
            }
            (Register::Global(g0), Register::Global(g1)) => {
                self.global.swap(g0 as usize, g1 as usize);
            }
            (Register::Local(l), Register::Global(g))
            | (Register::Global(g), Register::Local(l)) => {
                let temp = self.current_local[l as usize].clone();
                self.current_local[l as usize] = self.global[g as usize].clone();
                self.global[g as usize] = temp;
            }
        }
    }
}

impl Display for Registers {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        /* Print global registers */

        write!(fmt, "\n\t\tGlobals {{")?;
        for i in 0..self.global.len() {
            write!(fmt, "{}: {}, ", i, &self.global[i])?
        }
        write!(fmt, "}};")?;

        /* Print current local registers */

        write!(fmt, "\n\t\tLocals {{")?;
        for i in 0..self.current_local.len() {
            write!(fmt, "{}: {}, ", i, &self.current_local[i])?
        }
        write!(fmt, "}};")?;

        /* Print saved local registers */

        write!(fmt, "\n\t\tSaved Locals {{")?;
        for (i, l) in self.local.iter().enumerate() {
            write!(fmt, "\n\t\t\t{}: ", i)?;
            write!(fmt, "Locals {{")?;
            for (i, r) in l.iter().enumerate() {
                write!(fmt, "{}: {}, ", i, r)?;
            }
            write!(fmt, "}};")?;
        }
        if !self.local.is_empty() {
            write!(fmt, "\n\t\t")?;
        }
        write!(fmt, "}}")
    }
}
