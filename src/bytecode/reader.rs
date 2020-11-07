use anyhow::Error;
use binread::io::Cursor;
use binread::BinRead;
use binread::BinReaderExt;
use std::path::PathBuf;

#[derive(Debug, BinRead)]
#[br(big, magic = b"FOR1")]
pub struct BEAM {
    size: u32,
    #[br(map = |val: u32|  std::str::from_utf8(&val.to_ne_bytes()).unwrap().to_string())]
    name: String,
    #[br(count = 1)]
    chunks: Vec<Chunk>,
}

#[derive(Debug, BinRead)]
#[br()]
pub enum Chunk {
    #[br(magic = b"AtU8")]
    AtU8 {
        size: u32,
        #[br(map = |val: [u8;4]| u32::from_be_bytes(val))]
        count: u32,
        #[br(count = count)]
        atoms: Vec<Atom>,
    },
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
    pub fn from_file(_file: PathBuf) -> Result<BEAM, Error> {
        let bytecode: &[u8] = include_bytes!("../../tests/holder.beam");
        let mut reader = Cursor::new(bytecode);
        let beam: BEAM = reader.read_ne().unwrap();
        print!("{:?}\n", beam);
        Ok(beam)
    }
}
