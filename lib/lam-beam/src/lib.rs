pub mod beam_reader;
pub mod byteops;
pub mod compact_term_reader;

pub use self::beam_reader::*;
pub use self::byteops::*;
pub use self::compact_term_reader::*;

pub type ExternalTerm = eetf::Term;
pub mod external_term {
    pub use eetf::*;
}
