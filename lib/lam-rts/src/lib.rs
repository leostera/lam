#[export_name = "lam_rts__start"]
pub extern "C" fn start(data: *const u8, size: usize) {
    let data: &[u8] = unsafe { std::slice::from_raw_parts(data, size) };
    let program: lam::program::Program = bincode::deserialize(&data).unwrap();
    run(program)
}

pub fn run(program: lam::program::Program) {
    println!("Size: {}", program.instructions.len());
    for instr in program.instructions {
        println!("{:?}", instr);
    }
}
