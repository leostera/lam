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
    LitT(ChunkData<LiteralTable>),

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
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    data: Vec<Function>,
}

#[derive(Debug, BinRead)]
pub struct Function {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    arity: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    offset: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    nfree: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    ouniq: u32,
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
pub struct LiteralTable {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    uncompressed_size: u32,
    #[br(
        count = size - 4,
        parse_with = LiteralTable::deflate_and_parse,
        args(size - 4, uncompressed_size)
    )]
    data: Vec<eetf::Term>,
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

        println!("{:?}", inflated);
        println!("{:?}", data);

        for _ in 0..count - 1 {
            println!("{:?}", cursor);
            let term = eetf::Term::decode(&mut cursor).unwrap();
            literals.push(term);
            cursor.set_position(cursor.position() + 4);
        }

        Ok(literals)
    }
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
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    data: Vec<Import>,
}

#[derive(Debug, BinRead)]
pub struct Import {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    module_atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    fun_atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    arity: u32,
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
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    count: u32,
    #[br(count = count)]
    data: Vec<Export>,
}

#[derive(Debug, BinRead)]
pub struct Export {
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    atom_index: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    arity: u32,
    #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
    label: u32,
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
