use lam_beam::external_term;
use lam_beam::{
    AtomTable, Chunk, CodeTable, CompactTerm, ExportTable, ExternalTerm, ImportTable, LiteralTable,
    OpCode, BEAM,
};
use lam_emu::{
    FnCall, FunctionLabel, Instruction, List, Literal, Module, Program, Register, Value,
};

#[derive(Debug, Clone, Default)]
#[repr(C)]
pub struct ModuleTranslator {
    beam_export_table: ExportTable,
    beam_import_table: ImportTable,
    beam_atom_table: AtomTable,
    beam_code_table: CodeTable,
    beam_literal_table: LiteralTable,
    lam_module: Module,
}

/// The translator module takes a number of BEAM bytecode structures and turns
/// them into a Program runnable by the emulator.
///
/// To do this, it needs to traverse all the instructions of the BEAM bytecodes,
/// and build up the appropriate modules, and functions, as well as pointing
/// to the _main_ function that the program will execute first.
///
impl ModuleTranslator {
    pub fn from_bytecode(&mut self, beam: BEAM) -> &mut ModuleTranslator {
        for chunk in beam.chunks() {
            match chunk {
                Chunk::AtU8(chunk_data) => self.beam_atom_table = chunk_data.data.clone(),
                Chunk::Atom(chunk_data) => self.beam_atom_table = chunk_data.data.clone(),
                Chunk::Code(chunk_data) => self.beam_code_table = chunk_data.data.clone(),
                Chunk::LitT(chunk_data) => self.beam_literal_table = chunk_data.data.clone(),
                Chunk::ExpT(chunk_data) => self.beam_export_table = chunk_data.data.clone(),
                Chunk::ImpT(chunk_data) => self.beam_import_table = chunk_data.data.clone(),
                _ => (),
            }
        }
        self.lam_module = self.build_module();
        self
    }

    pub fn module(&self) -> Module {
        self.lam_module.clone()
    }

    fn build_module(&self) -> Module {
        let mut module = Module::default();
        module.name = self.beam_atom_table.atoms[0].name.clone();

        for _ in 1..=(self.beam_code_table.label_count as u8) {
            module.labels.push(FunctionLabel::default());
        }

        let mut current_label: u8 = 0;

        /* NOTE(@ostera): the atom table really begins at 1 instead of 0, but
         * when we read it, we index it from 0, so all atom indices are off by 1 */
        for export in self.beam_export_table.exports.iter() {
            let atom_idx = export.atom_index - 1;

            let fn_name = &self.beam_atom_table.atoms[atom_idx as usize].name;
            let fn_arity = export.arity as u8;
            let fn_first_label = export.label as u8;

            let key = (fn_name.to_string(), fn_arity);
            module.functions.insert(key, fn_first_label);
        }

        /* We'll go over every instruction, batching them by the current label id */
        for (opcode, _arity, args) in self.beam_code_table.instructions() {
            match opcode {
                /* This OpCode defines the _beginning_ of a function section.
                 *
                 * If there is none, then we won't actually do anything in this
                 * function and you'll end up with an empty module. */
                OpCode::Label => {
                    current_label = args[0].clone().into();
                }

                /* If we are in a label, try to translate the instructions into
                 * something we can handle in the emulator. */
                _ if current_label > 0 => {
                    if let Some(lam_instr) = ModuleTranslator::mk_instr(
                        &opcode,
                        &args,
                        &self.beam_atom_table,
                        &self.beam_literal_table,
                        &self.beam_import_table,
                    ) {
                        module.labels[(current_label - 1) as usize]
                            .instructions
                            .push(lam_instr);
                    }
                }
                _ => (),
            }
        }

        module
    }

    pub fn mk_instr(
        opcode: &lam_beam::OpCode,
        args: &[lam_beam::CompactTerm],
        atom_table: &lam_beam::AtomTable,
        literal_table: &lam_beam::LiteralTable,
        import_table: &lam_beam::ImportTable,
    ) -> Option<Instruction> {
        /* using the args, look up the right values in the right tables */
        match opcode {
            OpCode::Move => {
                let from = ModuleTranslator::mk_value_of_compact_term(
                    args[0].clone(),
                    &atom_table,
                    &literal_table,
                );
                let to = ModuleTranslator::mk_reg(args[1].clone());
                Some(Instruction::Move(from, to))
            }
            OpCode::CallExtOnly => {
                let import_idx: u8 = args[1].clone().into();
                let import = &import_table.imports[import_idx as usize];

                let module = atom_table.atoms[(import.module_atom_index - 1) as usize]
                    .name
                    .to_string();
                let function = atom_table.atoms[(import.fun_atom_index - 1) as usize]
                    .name
                    .to_string();
                let arity = import.arity as u8;

                Some(Instruction::Call(FnCall::Qualified {
                    module,
                    function,
                    arity,
                }))
            }
            _ => None,
        }
    }

    pub fn mk_value_of_compact_term(
        x: CompactTerm,
        atom_table: &lam_beam::AtomTable,
        literal_table: &lam_beam::LiteralTable,
    ) -> Value {
        match x {
            CompactTerm::RegisterX(x) => Value::Register(Register::X(x)),
            CompactTerm::RegisterY(y) => Value::Register(Register::Y(y)),
            CompactTerm::Nil => Value::Literal(Literal::List(List::Nil)),
            CompactTerm::Integer(v) => ModuleTranslator::mk_int(v),
            CompactTerm::Character(c) => Value::Literal(Literal::Character(c)),
            CompactTerm::Atom(idx) => Value::Literal(Literal::Atom(
                atom_table.atoms[(idx - 1) as usize].name.to_string(),
            )),
            CompactTerm::Literal(idx)
            | CompactTerm::ExtendedLiteral(lam_beam::Value::Small(idx)) => {
                Value::Literal(ModuleTranslator::mk_literal_of_external_term(
                    &literal_table.literals[idx as usize],
                ))
            }
            CompactTerm::ExtendedLiteral(v) => ModuleTranslator::mk_int(v),
            _ => panic!(
                "Don't know how to turn CompactTerm {:?} into a lam_emu::Value",
                x
            ),
        }
    }

    pub fn mk_literal_of_external_term(x: &ExternalTerm) -> Literal {
        match x {
            ExternalTerm::List(external_term::List { elements }) => {
                let value = elements.iter().fold(List::Nil, |acc, el| {
                    List::Cons(
                        Box::new(ModuleTranslator::mk_literal_of_external_term(el)),
                        Box::new(acc),
                    )
                });
                Literal::List(value)
            }
            ExternalTerm::Atom(atom) => Literal::Atom(atom.to_string()),
            ExternalTerm::Binary(bin) => Literal::Binary(
                String::from_utf8(bin.bytes.clone())
                    .expect("Binary string had invalid utf-8 characters"),
            ),
            _ => panic!(
                "Don't know how to turn ExternalTerm {:?} into a lam_emu::Value",
                x
            ),
        }
    }

    pub fn mk_int(x: lam_beam::Value) -> Value {
        match x {
            lam_beam::Value::Small(y) => Value::Literal(Literal::Integer(y.into())),
            lam_beam::Value::Large(z) => Value::Literal(Literal::Integer(z)),
        }
    }

    pub fn mk_reg(x: CompactTerm) -> Register {
        match x {
            CompactTerm::RegisterX(x) => Register::X(x),
            CompactTerm::RegisterY(y) => Register::Y(y),
            _ => panic!("Tried to turn {:?} into a register", x),
        }
    }
}

#[derive(Debug, Clone, Default)]
#[repr(C)]
pub struct Translator {}

impl Translator {
    pub fn from_bytecode(&mut self, beams: Vec<BEAM>) -> lam_emu::Program {
        let mut modules = vec![];
        for beam in beams {
            let module = ModuleTranslator::default().from_bytecode(beam).module();
            modules.push(module);
        }
        Program::default().with_modules(modules)
    }
}
