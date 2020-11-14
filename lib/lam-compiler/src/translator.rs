use lam_beam::{AtomTable, Chunk, CodeTable, LiteralTable, OpCode, BEAM};
use lam_emu::{Instruction, Program, MFA};

#[derive(Debug, Clone, Default)]
pub struct Translator {
    bytecode: Vec<BEAM>,
}

impl Translator {
    pub fn with_bytecode(self, bytecode: Vec<BEAM>) -> Translator {
        Translator { bytecode, ..self }
    }

    pub fn to_program(self) -> Program {
        let instructions = self
            .bytecode
            .iter()
            .flat_map(|beam| Translator::beam_to_instructions(beam))
            .collect();

        Program::from_instructions(instructions)
    }

    pub fn beam_to_instructions(beam: &BEAM) -> Vec<Instruction> {
        let mut instrs = vec![];

        let mut atom_table = AtomTable::default();
        let mut code_table = CodeTable::default();
        let mut literal_table = LiteralTable::default();

        for chunk in beam.chunks() {
            match chunk {
                Chunk::AtU8(chunk_data) => atom_table = chunk_data.data.clone(),
                Chunk::Atom(chunk_data) => atom_table = chunk_data.data.clone(),
                Chunk::Code(chunk_data) => code_table = chunk_data.data.clone(),
                Chunk::LitT(chunk_data) => literal_table = chunk_data.data.clone(),
                _ => (),
            }
        }

        for (opcode, _arity, args) in code_table.instructions() {
            if let Some(instr) =
                Translator::mk_instr(opcode, args, &atom_table, &code_table, &literal_table)
            {
                instrs.push(instr)
            }
        }

        instrs
    }

    fn mk_instr(
        opcode: &lam_beam::OpCode,
        args: &std::vec::Vec<lam_beam::CompactTerm>,
        atom_table: &lam_beam::AtomTable,
        code_table: &lam_beam::CodeTable,
        literal_table: &lam_beam::LiteralTable,
    ) -> Option<Instruction> {
        /* using the args, look up the right values in the right tables */
        match opcode {
            OpCode::Allocate => Some(Instruction::Allocate(args[0].into(), args[1].into())),
            OpCode::CallExtOnly => Some(Instruction::CallExtOnly {
                arity: args[0].into(),
                mfa: {
                    let (module, function, arity) = args[1].into();
                    let module = atom_table.atoms[module as usize].name;
                    let function = atom_table.atoms[function as usize].name;
                    MFA {
                        module,
                        function,
                        arity,
                    }
                },
            }),
            OpCode::CallOnly => Some(Instruction::CallOnly {
                arity: args[0].into(),
                label: args[1].into(),
            }),
            OpCode::Label => Some(Instruction::Label { id: args[0].into() }),
            OpCode::Move => {
                let from = Translator::mk_value(args[0]);
                let to = Translator::mk_reg(args[1]);
                Some(Instruction::Move(from, to))
            }
            _ => None,
        }
    }

    fn mk_value(x: CompactTerm) -> Value {
        match x {
            RegisterX(x) => Value::Register(Register::X(x)),
            RegisterY(x) => Value::Register(Register::Y(y)),
            Nil => Value::Nil,
            Literal(idx) => (),
            Integer(Value) => (),
            Atom(idx) => (),
            Label(idx) => (),
            Character(u8) => (),
            List => (),
            RegisterFloat => (),
            ListAllocation => (),
            ExtendedLiteral(Value) => (),
        }
    }
}
