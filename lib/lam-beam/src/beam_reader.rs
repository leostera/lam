use anyhow::Error;
use binread::io::{Cursor, Read, Seek};
use binread::BinRead;
use binread::BinReaderExt;
use log::trace;
use std::path::PathBuf;

use super::byteops::OpCode;
use super::compact_term_reader::CompactTerm;

#[derive(Default, Debug, Clone, BinRead)]
#[br(big, magic = b"FOR1")]
pub struct BEAM {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    size: u32,
    #[br(little, map = |val: u32|  std::str::from_utf8(&val.to_ne_bytes()).unwrap().to_string())]
    name: String,
    #[br(count = size, parse_with = BEAM::parse_chunks)]
    chunks: Vec<Chunk>,
}

impl BEAM {
    pub fn size(&self) -> u32 {
        self.size
    }

    pub fn chunks(&self) -> std::slice::Iter<'_, Chunk> {
        self.chunks.iter()
    }

    fn parse_chunks<R: Read + Seek>(
        reader: &mut R,
        ro: &binread::ReadOptions,
        arg: (),
    ) -> binread::BinResult<Vec<Chunk>> {
        let mut chunks = vec![];

        while let Ok(mut chunk) = Chunk::read(reader) {
            chunk.after_parse(reader, ro, arg)?;
            chunks.push(chunk)
        }

        Ok(chunks)
    }
}

#[derive(Default, Debug, Clone, BinRead)]
pub struct ChunkData<T: BinRead<Args = (u32,)>> {
    #[br(map = |val: [u8;4]| std::cmp::max(0, i32::from_be_bytes(val)) as u32)]
    size: u32,
    #[br(count = size, args(size))]
    pub data: T,
    #[br(
        count = 4 * ( (size +4 -1) / 4) - size,
        map = |val: Vec<u8>| val.len() as u32
    )]
    offset: u32,
}

#[derive(Debug, Clone, BinRead)]
pub enum Chunk {
    #[br(magic = b"Abst")]
    Abst(ChunkData<AbstTable>),

    #[br(magic = b"AtU8")]
    AtU8(ChunkData<AtomTable>),

    #[br(magic = b"Atom")]
    Atom(ChunkData<AtomTable>),

    #[br(magic = b"Attr")]
    Attr(ChunkData<AttributeTable>),

    #[br(magic = b"CInf")]
    CInf(ChunkData<CInfTable>),

    #[br(magic = b"Code")]
    Code(ChunkData<CodeTable>),

    #[br(magic = b"Dbgi")]
    Dbgi(ChunkData<DbgiTable>),

    #[br(magic = b"Docs")]
    Docs(ChunkData<DocsTable>),

    #[br(magic = b"ExpT")]
    ExpT(ChunkData<ExportTable>),

    #[br(magic = b"FunT")]
    FunT(ChunkData<LambdaTable>),

    #[br(magic = b"ImpT")]
    ImpT(ChunkData<ImportTable>),

    #[br(magic = b"Line")]
    Line(ChunkData<LineTable>),

    #[br(magic = b"LitT")]
    LitT(ChunkData<LiteralTable>),

    #[br(magic = b"LocT")]
    LocT(ChunkData<LocalFunctionTable>),

    #[br(magic = b"StrT")]
    StrT(ChunkData<StringTable>),
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct AtomTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    pub atoms: Vec<Atom>,
}

pub type Instruction = (OpCode, u8, std::vec::Vec<CompactTerm>);

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct CodeTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    code_version: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    min_opcode: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    max_opcode: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub label_count: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub fun_count: u32,
    #[br(
        count = size - 5*4,
        parse_with = CodeTable::parse_into_terms,
        args(size - 5*4)
    )]
    instructions: Vec<Instruction>,
}

impl CodeTable {
    pub fn instructions(&self) -> std::slice::Iter<'_, Instruction> {
        self.instructions.iter()
    }

    fn parse_into_terms<R: Read + Seek>(
        reader: &mut R,
        _ro: &binread::ReadOptions,
        args: (u32,),
    ) -> binread::BinResult<Vec<Instruction>> {
        let (size,) = args;

        let mut buf = vec![0; size as usize];
        reader.read_exact(&mut buf).unwrap();

        let mut instructions = vec![];

        let data: Vec<u8> = buf.to_vec();
        let mut cursor = Cursor::new(&data[0..]);
        let mut opcode_buf: [u8; 1] = [0; 1];
        while let Ok(()) = cursor.read_exact(&mut opcode_buf) {
            let opcode: OpCode = opcode_buf[0].into();
            let arity: u8 = opcode.arity();
            trace!("#{:#010b} @ {:?}/{:?}", opcode_buf[0], opcode, arity);
            let mut args: Vec<CompactTerm> = vec![];
            for _ in 0..arity {
                let term = CompactTerm::decode(&mut cursor).expect("Could not decode compact term");
                args.push(term);
            }
            instructions.push((opcode, arity, args));
        }

        Ok(instructions)
    }
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct LambdaTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    pub functions: Vec<Lambda>,
}

#[derive(Default, Debug, Clone, BinRead)]
pub struct Lambda {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub arity: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub label: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub nfree: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub ouniq: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct LocalFunctionTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    pub data: Vec<LocalFunction>,
}

#[derive(Default, Debug, Clone, BinRead)]
pub struct LocalFunction {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub fun_atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub arity: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub label: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct DbgiTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct CInfTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct DocsTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct LiteralTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    uncompressed_size: u32,
    #[br(
        count = size - 4,
        parse_with = LiteralTable::deflate_and_parse,
        args(size - 4, uncompressed_size)
    )]
    pub literals: Vec<eetf::Term>,
}

impl LiteralTable {
    fn deflate_and_parse<R: Read + Seek>(
        reader: &mut R,
        _ro: &binread::ReadOptions,
        args: (u32, u32),
    ) -> binread::BinResult<Vec<eetf::Term>> {
        let mut literals = vec![];

        let (deflated_size, inflated_size) = args;

        let mut buf = vec![0; deflated_size as usize];
        reader.read_exact(&mut buf).unwrap();

        let mut deflater = flate2::read::ZlibDecoder::new(buf.as_slice());
        let mut inflated = vec![0; inflated_size as usize];
        deflater.read(&mut inflated).unwrap();

        let count = u32::from_be_bytes([inflated[0], inflated[1], inflated[2], inflated[3]]);

        let data: Vec<u8> = inflated[8..].to_vec();
        let mut cursor = Cursor::new(&data);

        for _ in 0..count {
            let term = eetf::Term::decode(&mut cursor).unwrap();
            literals.push(term);
            cursor.set_position(cursor.position() + 4);
        }

        Ok(literals)
    }
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct StringTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct LineTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct ImportTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    pub imports: Vec<Import>,
}

#[derive(Default, Debug, Clone, BinRead)]
pub struct Import {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub module_atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub fun_atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub arity: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct AbstTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct AttributeTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(import(size : u32))]
pub struct ExportTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    pub exports: Vec<Export>,
}

#[derive(Default, Debug, Clone, BinRead)]
pub struct Export {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub arity: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    pub label: u32,
}

#[derive(Default, Debug, Clone, BinRead)]
#[br(assert(size > 0))]
pub struct Atom {
    size: u8,
    #[br(count = size, map = |val: Vec<u8>|  std::str::from_utf8(&val.as_slice()).unwrap().to_string())]
    pub name: String,
}

#[derive(Default)]
pub struct Reader {}

impl Reader {
    pub fn from_file(file: &PathBuf) -> Result<BEAM, Error> {
        let bytecode = std::fs::read(file)?;
        let mut reader = Cursor::new(bytecode);
        let beam: BEAM = reader.read_ne().unwrap();
        Ok(beam)
    }
}
