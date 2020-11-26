use super::literal::*;
use std::cell::RefCell;
use std::collections::VecDeque;

pub type Message = Literal;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Mailbox {
    messages: RefCell<VecDeque<Message>>,
    current: RefCell<u32>,
}

impl Mailbox {
    pub fn new() -> Mailbox {
        Mailbox {
            messages: RefCell::new(VecDeque::default()),
            current: RefCell::new(0),
        }
    }

    pub fn deliver(&self, m: Message) {
        self.messages.borrow_mut().push_back(m);
    }

    pub fn drop_current(&self) {
        let idx = *self.current.borrow();
        self.messages.borrow_mut().remove(idx as usize);
    }

    pub fn peek_next(&self) -> Option<Message> {
        let idx = *self.current.borrow();
        if let Some(msg) = self.messages.borrow_mut().get(idx as usize) {
            (*self.current.borrow_mut()) += 1;
            Some(msg.clone())
        } else {
            None
        }
    }
}
