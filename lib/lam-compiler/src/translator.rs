use log::*;

use lam_beam::external_term;
use lam_beam::{
    AtomTable, Chunk, CodeTable, CompactTerm, ExportTable, ExternalTerm, ImportTable, LiteralTable,
    LocalFunctionTable, OpCode, BEAM,
};
use lam_emu::{
    FnCall, FnKind, FunctionLabel, Instruction, Label, List, Literal, Module, Program, Register,
    Spawn, Test, Value,
};

#[derive(Debug, Clone, Default)]
#[repr(C)]
pub struct ModuleTranslator {
    beam_local_fun_table: LocalFunctionTable,
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
        trace!("From Bytecode: {:#?}", beam);
        for chunk in beam.chunks() {
            match chunk {
                Chunk::AtU8(chunk_data) => self.beam_atom_table = chunk_data.data.clone(),
                Chunk::Atom(chunk_data) => self.beam_atom_table = chunk_data.data.clone(),
                Chunk::Code(chunk_data) => self.beam_code_table = chunk_data.data.clone(),
                Chunk::LitT(chunk_data) => self.beam_literal_table = chunk_data.data.clone(),
                Chunk::ExpT(chunk_data) => self.beam_export_table = chunk_data.data.clone(),
                Chunk::ImpT(chunk_data) => self.beam_import_table = chunk_data.data.clone(),
                Chunk::LocT(chunk_data) => self.beam_local_fun_table = chunk_data.data.clone(),
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

        for i in 0..self.beam_code_table.label_count {
            module.labels.push(FunctionLabel::new(i));
        }

        let mut current_label: Label = 0;

        /* NOTE(@ostera): the atom table really begins at 1 instead of 0, but
         * when we read it, we index it from 0, so all atom indices are off by 1 */
        for export in self.beam_export_table.exports.iter() {
            let atom_idx = export.atom_index - 1;

            let fn_name = &self.beam_atom_table.atoms[atom_idx as usize].name;
            let fn_arity = export.arity;
            let fn_first_label = export.label - 1;

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
                    current_label -= 1;
                }

                /* If we are in a label, try to translate the instructions into
                 * something we can handle in the emulator. */
                _ => {
                    if let Some(lam_instr) = ModuleTranslator::mk_instr(
                        &module,
                        &opcode,
                        &args,
                        &self.beam_local_fun_table,
                        &self.beam_atom_table,
                        &self.beam_literal_table,
                        &self.beam_import_table,
                    ) {
                        trace!("=> {:?}", lam_instr);
                        module.labels[current_label as usize]
                            .instructions
                            .push(lam_instr);
                    }
                }
            }
        }

        module
    }

    pub fn mk_instr(
        module: &lam_emu::Module,
        opcode: &lam_beam::OpCode,
        args: &[lam_beam::CompactTerm],
        local_fun_table: &lam_beam::LocalFunctionTable,
        atom_table: &lam_beam::AtomTable,
        literal_table: &lam_beam::LiteralTable,
        import_table: &lam_beam::ImportTable,
    ) -> Option<Instruction> {
        /* using the args, look up the right values in the right tables */
        trace!("{:?} <- {:?}", opcode, args);
        match opcode {
            ///////////////////////////////////////////////////////////////////
            //
            //  Register Machine instructions
            //
            OpCode::Move => {
                let from = ModuleTranslator::mk_value_of_compact_term(
                    args[0].clone(),
                    &atom_table,
                    &literal_table,
                );
                let to = ModuleTranslator::mk_reg(args[1].clone());
                Some(Instruction::Move(from, to))
            }

            OpCode::Swap => {
                let a = ModuleTranslator::mk_reg(args[0].clone());
                let b = ModuleTranslator::mk_reg(args[1].clone());
                Some(Instruction::Swap(a, b))
            }

            ///////////////////////////////////////////////////////////////////
            //
            //  Working with the Heap
            /*
            OpCode::AllocateHeap | OpCode::TestHeap | OpCode::Allocate => {
                Some(Instruction::Allocate {
                    words: args[0].clone().into(),
                    keep_registers: args[1].clone().into(),
                })
            }

            OpCode::Deallocate => Some(Instruction::Deallocate {
                words: args[0].clone().into(),
            }),
            */
            // NOTE(@ostera): temporarily disable this so the traces are smaller, we aren't using
            // the allocation anyway
            OpCode::AllocateHeap | OpCode::TestHeap | OpCode::Allocate | OpCode::Deallocate => None,

            ///////////////////////////////////////////////////////////////////
            //
            //  Control-Flow
            //
            OpCode::Return => Some(Instruction::Return),

            OpCode::Badmatch => Some(Instruction::Badmatch),

            ///////////////////////////////////////////////////////////////////
            //
            //  Function Calls
            //
            OpCode::CallFun => {
                let arity = args[0].clone().into();
                Some(Instruction::Call(
                    FnCall::ApplyLambda {
                        arity,
                        register: Register::Global(arity),
                    },
                    FnKind::User,
                ))
            }

            OpCode::Call | OpCode::CallOnly | OpCode::CallLast => {
                let arity: u32 = args[0].clone().into();
                let label: u32 = args[1].clone().into();
                Some(Instruction::Call(
                    FnCall::Local {
                        module: module.name.clone(),
                        label: label - 1,
                        arity,
                    },
                    FnKind::User,
                ))
            }

            /* Translate fully qualified calls */
            opcode @ OpCode::CallExt
            | opcode @ OpCode::CallExtOnly
            | opcode @ OpCode::CallExtLast => {
                let (module, function, arity) = ModuleTranslator::mk_mfa_from_imports(
                    args[1].clone().into(),
                    &import_table,
                    &atom_table,
                );
                let kind = match module.as_str() {
                    "binary" | "file" | "io" | "erlang" => FnKind::Native,
                    _ => FnKind::User,
                };
                Some(match (module.as_str(), function.as_str(), arity) {
                    ("erlang", "spawn", 1) => Instruction::Spawn(Spawn::Lambda {
                        register: Register::Global(0),
                    }),
                    (_, _, _) => {
                        let call = FnCall::Qualified {
                            module,
                            function,
                            arity,
                        };
                        match opcode {
                            OpCode::CallExt => Instruction::Call(call, kind),
                            _ => Instruction::TailCall(call, kind),
                        }
                    }
                })
            }

            OpCode::GcBif1 => {
                let (module, function, arity) = ModuleTranslator::mk_mfa_from_imports(
                    args[2].clone().into(),
                    &import_table,
                    &atom_table,
                );
                let a = ModuleTranslator::mk_value_of_compact_term(
                    args[3].clone(),
                    &atom_table,
                    &literal_table,
                );
                let dest = ModuleTranslator::mk_reg(args[4].clone());
                let bif = FnCall::BuiltIn {
                    module,
                    function,
                    arity,
                    arguments: vec![a],
                    destination: dest,
                };
                Some(Instruction::Call(bif, FnKind::Native))
            }

            OpCode::GcBif2 => {
                let (module, function, arity) = ModuleTranslator::mk_mfa_from_imports(
                    args[2].clone().into(),
                    &import_table,
                    &atom_table,
                );
                let a = ModuleTranslator::mk_value_of_compact_term(
                    args[3].clone(),
                    &atom_table,
                    &literal_table,
                );
                let b = ModuleTranslator::mk_value_of_compact_term(
                    args[4].clone(),
                    &atom_table,
                    &literal_table,
                );
                let dest = ModuleTranslator::mk_reg(args[5].clone());
                let bif = FnCall::BuiltIn {
                    module,
                    function,
                    arity,
                    arguments: vec![a, b],
                    destination: dest,
                };
                Some(Instruction::Call(bif, FnKind::Native))
            }

            OpCode::GcBif3 => {
                let (module, function, arity) = ModuleTranslator::mk_mfa_from_imports(
                    args[2].clone().into(),
                    &import_table,
                    &atom_table,
                );
                let a = ModuleTranslator::mk_value_of_compact_term(
                    args[3].clone(),
                    &atom_table,
                    &literal_table,
                );
                let b = ModuleTranslator::mk_value_of_compact_term(
                    args[4].clone(),
                    &atom_table,
                    &literal_table,
                );
                let c = ModuleTranslator::mk_value_of_compact_term(
                    args[5].clone(),
                    &atom_table,
                    &literal_table,
                );
                let dest = ModuleTranslator::mk_reg(args[6].clone());
                let bif = FnCall::BuiltIn {
                    module,
                    function,
                    arity,
                    arguments: vec![a, b, c],
                    destination: dest,
                };
                Some(Instruction::Call(bif, FnKind::Native))
            }

            OpCode::Bif0 => {
                let (module, function, arity) = ModuleTranslator::mk_mfa_from_imports(
                    args[0].clone().into(),
                    &import_table,
                    &atom_table,
                );

                trace!("Bif0 MFA: {:?}", (&module, &function, &arity));

                match function.as_str() {
                    "self" => {
                        let reg = ModuleTranslator::mk_reg(args[1].clone());
                        Some(Instruction::PidSelf(reg))
                    }
                    _ => None,
                }
            }

            ///////////////////////////////////////////////////////////////////
            //
            //  Tests
            //
            OpCode::IsGe => {
                let label: u32 = args[0].clone().into();
                let a = ModuleTranslator::mk_value_of_compact_term(
                    args[1].clone(),
                    &atom_table,
                    &literal_table,
                );
                let b = ModuleTranslator::mk_value_of_compact_term(
                    args[2].clone(),
                    &atom_table,
                    &literal_table,
                );
                Some(Instruction::Test(
                    label - 1,
                    Test::IsGreaterOrEqualThan(a, b),
                ))
            }

            OpCode::IsEqExact => {
                // {test,is_eq_exact,{f,7},[{x,0},{integer,0}]}.
                let label: u32 = args[0].clone().into();
                let a = ModuleTranslator::mk_value_of_compact_term(
                    args[1].clone(),
                    &atom_table,
                    &literal_table,
                );
                let b = ModuleTranslator::mk_value_of_compact_term(
                    args[2].clone(),
                    &atom_table,
                    &literal_table,
                );
                Some(Instruction::Test(label - 1, Test::Equals(a, b)))
            }

            OpCode::IsNil => {
                let label: u32 = args[0].clone().into();
                let a = ModuleTranslator::mk_value_of_compact_term(
                    args[1].clone(),
                    &atom_table,
                    &literal_table,
                );
                Some(Instruction::Test(label - 1, Test::IsNil(a)))
            }

            OpCode::IsNonemptyList => {
                let label: u32 = args[0].clone().into();
                let a = ModuleTranslator::mk_value_of_compact_term(
                    args[1].clone(),
                    &atom_table,
                    &literal_table,
                );
                Some(Instruction::Test(label - 1, Test::IsNonEmptyList(a)))
            }

            OpCode::IsTaggedTuple => {
                // {test,is_tagged_tuple,{f,8},[{x,0},2,{atom,some}]}.
                let label: u32 = args[0].clone().into();

                let value = ModuleTranslator::mk_value_of_compact_term(
                    args[1].clone(),
                    &atom_table,
                    &literal_table,
                );

                let size: u32 = args[2].clone().into();

                let atom_idx: u32 = args[3].clone().into();
                let atom = atom_table.atoms[(atom_idx - 1) as usize].name.to_string();

                Some(Instruction::Test(
                    label - 1,
                    Test::IsTaggedTuple { value, size, atom },
                ))
            }

            OpCode::IsFunction2 => {
                // {test,is_function2,{f,1},[{x,0},{integer,1}]}.
                let label: u32 = args[0].clone().into();

                let fun = ModuleTranslator::mk_reg(args[1].clone());
                let arity: u32 = args[2].clone().into();

                Some(Instruction::Test(
                    label - 1,
                    Test::IsFunctionWithArity { fun, arity },
                ))
            }

            ///////////////////////////////////////////////////////////////////
            //
            //  Creating Values
            //
            OpCode::PutList => {
                // {put_list,{x,2},nil,{x,1}}.
                let head = ModuleTranslator::mk_value_of_compact_term(
                    args[0].clone(),
                    &atom_table,
                    &literal_table,
                );
                let tail = ModuleTranslator::mk_value_of_compact_term(
                    args[1].clone(),
                    &atom_table,
                    &literal_table,
                );
                let target = ModuleTranslator::mk_reg(args[2].clone());
                Some(Instruction::ConsList { target, head, tail })
            }

            OpCode::GetList => {
                // {get_list,{x,0},{x,1},{x,2}}.
                let list = ModuleTranslator::mk_reg(args[0].clone());
                let head = ModuleTranslator::mk_reg(args[1].clone());
                let tail = ModuleTranslator::mk_reg(args[2].clone());
                Some(Instruction::SplitList { list, head, tail })
            }

            OpCode::GetTl => {
                let list = ModuleTranslator::mk_reg(args[0].clone());
                let tail = ModuleTranslator::mk_reg(args[1].clone());
                Some(Instruction::SplitListTail { list, tail })
            }

            OpCode::GetHd => {
                let list = ModuleTranslator::mk_reg(args[0].clone());
                let head = ModuleTranslator::mk_reg(args[1].clone());
                Some(Instruction::SplitListHead { list, head })
            }

            OpCode::GetTupleElement => {
                // {get_tuple_element,{x,0},1,{x,0}}.
                let tuple = ModuleTranslator::mk_reg(args[0].clone());
                let element: u32 = args[1].clone().into();
                let target = ModuleTranslator::mk_reg(args[2].clone());
                Some(Instruction::GetTupleElement {
                    tuple,
                    element,
                    target,
                })
            }

            OpCode::MakeFun | OpCode::MakeFun2 => {
                //         label  ? ? arity
                // {make_fun2,{f,16},0,0,2}.
                let local_fun = {
                    let lf_idx: u32 = args[0].clone().into();
                    local_fun_table.data[lf_idx as usize].clone()
                };
                trace!("MakeFun2: {:?}", local_fun);
                Some(Instruction::MakeLambda {
                    module: module.name.clone(),
                    first_label: local_fun.label - 1,
                    arity: 0,
                    environment_size: local_fun.arity,
                })
            }

            ///////////////////////////////////////////////////////////////////
            //
            //  Message Passing and Processes
            //
            OpCode::LoopRec => {
                let label: u32 = args[0].clone().into();
                let on_mailbox_empty = label - 1;

                let message = ModuleTranslator::mk_reg(args[1].clone());

                Some(Instruction::PeekMessage {
                    on_mailbox_empty,
                    message,
                })
            }

            OpCode::Wait => {
                let label: u32 = args[0].clone().into();
                let label = label - 1;
                Some(Instruction::Sleep(label))
            }

            OpCode::LoopRecEnd => {
                let label: u32 = args[0].clone().into();
                let label = label - 1;
                Some(Instruction::Jump(label))
            }

            OpCode::RemoveMessage => Some(Instruction::RemoveMessage),

            OpCode::Send => Some(Instruction::Send {
                process: Value::Register(Register::Global(0)),
                message: Value::Register(Register::Global(1)),
            }),

            ///////////////////////////////////////////////////////////////////
            //
            //  Other!
            //
            OpCode::Trim | OpCode::FuncInfo | OpCode::Line | OpCode::IntCodeEnd => None,
            x => panic!("Unsupported instruction: {:?}", x),
        }
    }

    pub fn mk_value_of_compact_term(
        x: CompactTerm,
        atom_table: &lam_beam::AtomTable,
        literal_table: &lam_beam::LiteralTable,
    ) -> Value {
        let res = match x.clone() {
            CompactTerm::RegisterX(x) => Value::Register(Register::Global(x)),
            CompactTerm::RegisterY(y) => Value::Register(Register::Local(y)),
            CompactTerm::Nil => Value::Literal(Literal::List(List::Nil)),
            CompactTerm::Integer(v) => ModuleTranslator::mk_int(v),
            CompactTerm::Character(c) => Value::Literal(Literal::Character(c as u8)),
            CompactTerm::Atom(idx) => Value::Literal(Literal::Atom(
                atom_table.atoms[(idx - 1) as usize].name.to_string(),
            )),
            CompactTerm::Literal(y) => Value::Literal(Literal::Integer(y.into())),
            CompactTerm::ExtendedLiteral(lam_beam::Value::Small(idx)) => {
                Value::Literal(ModuleTranslator::mk_literal_of_external_term(
                    &literal_table.literals[idx as usize],
                ))
            }
            CompactTerm::ExtendedLiteral(v) => ModuleTranslator::mk_int(v),
            _ => panic!(
                "Don't know how to turn CompactTerm {:?} into a lam_emu::Value",
                x
            ),
        };
        trace!("mk_value_of_compact({:?}) -> {:?}", x, res);
        res
    }

    pub fn mk_literal_of_external_term(x: &ExternalTerm) -> Literal {
        let res = match x.clone() {
            ExternalTerm::List(external_term::List { elements }) => {
                Literal::List(elements.iter().rev().fold(List::Nil, |acc, el| {
                    List::Cons(
                        Box::new(ModuleTranslator::mk_literal_of_external_term(el)),
                        Box::new(acc),
                    )
                }))
                .into()
            }
            ExternalTerm::Atom(atom) => Literal::Atom(atom.to_string()),
            ExternalTerm::Binary(bin) => Literal::Binary(
                String::from_utf8(bin.bytes).expect("Binary string had invalid utf-8 characters"),
            ),
            ExternalTerm::FixInteger(external_term::FixInteger { value }) => {
                Literal::Integer(value.into())
            }
            _ => panic!(
                "Don't know how to turn ExternalTerm {:?} into a lam_emu::Value",
                x
            ),
        };
        trace!("mk_lit_of_ext({:?}) -> {:?}", x, res);
        res
    }

    pub fn mk_int(x: lam_beam::Value) -> Value {
        let res = match x.clone() {
            lam_beam::Value::Small(y) => Value::Literal(Literal::Integer(y.into())),
            lam_beam::Value::Large(z) => Value::Literal(Literal::Integer(z)),
        };
        trace!("mk_int({:?}) -> {:?}", x, res);
        res
    }

    pub fn mk_reg(x: CompactTerm) -> Register {
        let res = match x {
            CompactTerm::RegisterX(x) => Register::Global(x),
            CompactTerm::RegisterY(y) => Register::Local(y),
            _ => panic!("Tried to turn {:?} into a register", x),
        };
        trace!("mk_reg({:?}) -> {:?}", x, res);
        res
    }

    pub fn mk_mfa_from_imports(
        import_idx: u32,
        import_table: &ImportTable,
        atom_table: &AtomTable,
    ) -> (String, String, u32) {
        let import = &import_table.imports[import_idx as usize];

        let module = atom_table.atoms[(import.module_atom_index - 1) as usize]
            .name
            .to_string();
        let function = atom_table.atoms[(import.fun_atom_index - 1) as usize]
            .name
            .to_string();
        let arity = import.arity;

        (module, function, arity)
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
