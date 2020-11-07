use anyhow::Error;
use binread::io::{Cursor, Read, Seek};
use binread::BinRead;
use binread::BinReaderExt;
use std::path::PathBuf;

#[derive(Debug, BinRead)]
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
    fn parse_chunks<R: Read + Seek>(
        reader: &mut R,
        ro: &binread::ReadOptions,
        arg: (),
    ) -> binread::BinResult<Vec<Chunk>> {
        let mut chunks = vec![];

        loop {
            match Chunk::read(reader) {
                Ok(mut chunk) => {
                    chunk.after_parse(reader, ro, arg)?;
                    chunks.push(chunk)
                }
                Err(_) => break,
            }
        }

        Ok(chunks)
    }
}

#[derive(Debug, BinRead)]
pub struct ChunkData<T: BinRead<Args = (u32,)>> {
    #[br(map = |val: [u8;4]| std::cmp::max(0, i32::from_be_bytes(val)) as u32)]
    size: u32,
    #[br(count = size, args(size))]
    data: T,
    #[br(
        count = 4 * ( (size +4 -1) / 4) - size,
        map = |val: Vec<u8>| val.len() as u32
    )]
    offset: u32,
}

#[derive(Debug, BinRead)]
pub enum Chunk {
    #[br(magic = b"Abst")]
    Abst(ChunkData<AbstTable>),

    #[br(magic = b"AtU8")]
    AtU8(ChunkData<AtomTable>),

    #[br(magic = b"Atom")]
    Atom(ChunkData<AtomTable>),

    #[br(magic = b"Attr")]
    Attr(ChunkData<AttrTable>),

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
    FunT(ChunkData<FunTable>),

    #[br(magic = b"ImpT")]
    ImpT(ChunkData<ImportTable>),

    #[br(magic = b"Line")]
    Line(ChunkData<LineTable>),

    #[br(magic = b"LitT")]
    LitT(ChunkData<LitTable>),

    #[br(magic = b"LocT")]
    LocT(ChunkData<LocTable>),

    #[br(magic = b"StrT")]
    StrT(ChunkData<StrTable>),
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct AtomTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    atoms: Vec<Atom>,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct CodeTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    code_version: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    max_opcode: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    label_count: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    fun_count: u32,
    #[br(count = size - 4*4, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct FunTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}
#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct LocTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct DbgiTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct CInfTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct DocsTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct LitTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct StrTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct LineTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct ImportTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct AbstTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct AttrTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(import(size : u32))]
pub struct ExportTable {
    #[br(count = size, map = |v: Vec<u8>| v.len() as u32)]
    data: u32,
}

#[derive(Debug, BinRead)]
#[br(assert(size > 0))]
pub struct Atom {
    size: u8,
    #[br(count = size, map = |val: Vec<u8>|  std::str::from_utf8(&val.as_slice()).unwrap().to_string())]
    name: String,
}

#[derive(Default)]
pub struct Reader {}

impl Reader {
    pub fn from_file(file: PathBuf) -> Result<BEAM, Error> {
        let bytecode = std::fs::read(file)?;
        let mut reader = Cursor::new(bytecode);
        let beam: BEAM = reader.read_ne().unwrap();
        print!("{:#?}\n", beam);
        Ok(beam)
    }
}
