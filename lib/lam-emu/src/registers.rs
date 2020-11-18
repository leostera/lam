use super::bytecode::*;
use super::literal::*;
use std::collections::VecDeque;

use log::*;

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub struct Registers {
    global: Vec<Value>,
    local: VecDeque<Vec<Value>>,
    current_local: Vec<Value>,
}

impl Registers {
    pub fn new() -> Registers {
        let empty: Vec<Value> = [0; 32].to_vec().iter().map(|_| Value::Nil).collect();
        Registers {
            global: empty.clone(),
            local: VecDeque::new(),
            current_local: empty,
        }
    }

    pub fn clear(&mut self, r: Register) -> &mut Registers {
        self.put(r, Value::Nil);
        self
    }

    pub fn clear_globals(&mut self, n: u32) -> &mut Registers {
        for i in (n as usize)..self.global.len() {
            self.global[i] = Value::Nil;
        }
        self
    }

    pub fn put_global(&mut self, n: u32, v: Value) -> &mut Registers {
        self.global[n as usize] = v.clone();
        self
    }

    pub fn put(&mut self, r: Register, v: Value) -> &mut Registers {
        match r {
            Register::Local(l) => self.current_local[l as usize] = v.clone(),
            Register::Global(g) => self.global[g as usize] = v.clone(),
        }
        self
    }

    pub fn get(&self, r: &Register) -> Value {
        trace!("fetching register: {:?}", r);
        match r {
            Register::Local(l) => self.current_local[*l as usize].clone(),
            Register::Global(g) => self.global[*g as usize].clone(),
        }
    }

    pub fn get_many_literals_from_global_range(&self, from: u32, to: u32) -> Vec<Literal> {
        self.global[from as usize..to as usize]
            .iter()
            .map(|v| self.get_literal_from_value(v))
            .collect()
    }

    pub fn get_literal_from_value(&self, v: &Value) -> Literal {
        trace!("fetching value: {:?}", v);
        match v {
            Value::Literal(l) => l.clone(),
            Value::Register(reg) => self.get_literal_from_value(&self.get(reg)),
            Value::Nil => Literal::List(List::Nil),
        }
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
