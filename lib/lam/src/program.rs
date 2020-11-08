#[derive(Debug, Clone)]
#[repr(C)]
pub enum Instruction {
    Allocate {},
    Noop,
}
