#[export_name = "lam_rts__start"]
pub extern "C" fn start(data: *const u8, size: usize) {
    let program: &[lam::program::Instruction] = unsafe {
        std::slice::from_raw_parts(
            data.cast::<lam::program::Instruction>(),
            size / std::mem::size_of::<lam::program::Instruction>(),
        )
    };
    run(program)
}

pub fn run(program: &[lam::program::Instruction]) {
    println!("Size: {}", program.len());
    println!("Bytecode: {:?}", program);
}
