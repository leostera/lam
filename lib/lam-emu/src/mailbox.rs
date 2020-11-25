use super::literal::*;
use std::collections::VecDeque;

pub type Message = Literal;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Mailbox {
    messages: VecDeque<Message>,
    current: u32,
}

impl Mailbox {
    pub fn new() -> Mailbox {
        Mailbox {
            messages: VecDeque::default(),
            current: 0,
        }
    }

    pub fn deliver(&mut self, m: Message) {
        self.messages.push_back(m);
    }

    pub fn drop_current(&mut self) -> &mut Mailbox {
        self.messages.remove(self.current as usize);
        self
    }

    pub fn peek_next(&mut self) -> Option<&Message> {
        let msg = self.messages.get(self.current as usize);
        self.current += 1;
        msg
    }
}
