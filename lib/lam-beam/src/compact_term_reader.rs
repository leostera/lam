use anyhow::Error;
use byteorder::ReadBytesExt;
/**
 * This module is heavily inspired from @kvakvs/ErlangRT's compat_term.rs
 * and @sile/eetf's codec.rs
 *
 * Thanks to both of you for the inspiration and guidance :)
 */
use log::trace;
use num_bigint::BigInt;
use std::io;

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum SimpleTag {
    Literal = 0b000,
    Integer = 0b001,
    Atom = 0b010,
    RegisterX = 0b011,
    RegisterY = 0b100,
    Label = 0b101,
    Character = 0b110,
    Extended = 0b111,
}

impl Into<SimpleTag> for u8 {
    fn into(self) -> SimpleTag {
        match self {
            0b000 => SimpleTag::Literal,
            0b001 => SimpleTag::Integer,
            0b010 => SimpleTag::Atom,
            0b011 => SimpleTag::RegisterX,
            0b100 => SimpleTag::RegisterY,
            0b101 => SimpleTag::Label,
            0b110 => SimpleTag::Character,
            0b111 => SimpleTag::Extended,
            _ => panic!(
                "Invalid u8->SimpleTag conversion: {:?} is not a valid tag",
                self
            ),
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum ExtendedTag {
    List = 0b0001_0111,
    RegisterFloat = 0b0010_0111,
    ListAllocation = 0b0011_0111,
    ExtendedLiteral = 0b0100_0111,
}

impl Into<ExtendedTag> for u8 {
    fn into(self) -> ExtendedTag {
        match self {
            0b0001_0111 => ExtendedTag::List,
            0b0010_0111 => ExtendedTag::RegisterFloat,
            0b0011_0111 => ExtendedTag::ListAllocation,
            0b0100_0111 => ExtendedTag::ExtendedLiteral,
            _ => panic!(
                "Invalid u8->ExtendedTag conversion: {:?} is not a valid tag",
                self
            ),
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Value {
    Small(u32),
    Large(BigInt),
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum TagKind {
    Simple(SimpleTag, Value),
    Extended(ExtendedTag, Value),
}

impl Into<CompactTerm> for TagKind {
    fn into(self) -> CompactTerm {
        match self {
            TagKind::Simple(SimpleTag::Literal, Value::Small(x)) => CompactTerm::Literal(x),
            TagKind::Simple(SimpleTag::Integer, value) => CompactTerm::Integer(value),
            TagKind::Simple(SimpleTag::Atom, Value::Small(0)) => CompactTerm::Nil,
            TagKind::Simple(SimpleTag::Atom, Value::Small(index)) => CompactTerm::Atom(index),
            TagKind::Simple(SimpleTag::RegisterX, Value::Small(x)) => CompactTerm::RegisterX(x),
            TagKind::Simple(SimpleTag::RegisterY, Value::Small(y)) => CompactTerm::RegisterY(y),
            TagKind::Simple(SimpleTag::Label, Value::Small(l)) => CompactTerm::Label(l),
            TagKind::Simple(SimpleTag::Character, Value::Small(c)) => CompactTerm::Character(c),
            TagKind::Extended(ExtendedTag::ExtendedLiteral, value) => {
                CompactTerm::ExtendedLiteral(value)
            }
            _ => panic!("Unsupported TagKind: {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum CompactTerm {
    Nil,
    Literal(u32),
    Integer(Value),
    Atom(u32),
    RegisterX(u32),
    RegisterY(u32),
    Label(u32),
    Character(u32),
    List,
    RegisterFloat,
    ListAllocation,
    ExtendedLiteral(Value),
}

impl CompactTerm {
    pub fn decode<R: io::Read>(reader: R) -> Result<CompactTerm, Error> {
        Decoder::new(reader).decode()
    }
}

pub struct Decoder<R> {
    reader: R,
}

impl<R: io::Read> Decoder<R> {
    pub fn new(reader: R) -> Self {
        Decoder { reader }
    }

    pub fn decode(mut self) -> Result<CompactTerm, Error> {
        let byte: u8 = self.reader.read_u8()?;

        /* First 3 bits indicate the tag of this value */
        let tag = byte & 0b111;

        let tagged_value = if tag < SimpleTag::Extended as u8 {
            /* The 4th bit indicates if the _value_ is extended */
            let is_small_value = 0 == (byte & 0b0000_1000);
            let is_large_value = 0 == (byte & 0b0001_0000);

            let value = if is_small_value {
                Value::Small((byte >> 4).into())
            } else if is_large_value {
                let shift_value = ((byte as u32) & 0b1110_0000) << 3;
                let next_byte = self.reader.read_u8()? as u32;
                Value::Small(shift_value | next_byte)
            } else {
                /* here we'll read the amount of extra bytes to read first
                 * and then loop over until we're done reading that value.
                 * NOTE: the value is encoded with -2, so we add +2 to it
                 */
                let mut byte_count = (byte >> 5) as u32 + 2;

                /* there's a special case where the byte_count is 9, because
                 * it means the top 5 bits were all 1: 0b1111_1xxx
                 *
                 * This means we have to read another byte, and it MUST be a
                 * small value. It if isn't, we should panic.
                 *
                 * This small value we will add to the byte count.
                 */
                if byte_count == 9 {
                    let next_byte = self.reader.read_u8()?;
                    let is_small_value = 0 == (next_byte & 0b0000_1000);
                    let is_large_value = 0 == (next_byte & 0b0001_0000);
                    byte_count += if is_small_value {
                        (next_byte >> 4) as u32
                    } else if is_large_value {
                        let shift_value = ((byte as u32) & 0b1110_0000) << 3;
                        let next_byte = self.reader.read_u8()? as u32;
                        shift_value | next_byte
                    } else {
                        panic!("We were expecting a small or small-extended value!");
                    }
                }

                /* read all the extra bytes */
                let mut buf: Vec<u8> = vec![0; byte_count as usize];
                trace!("Reading {:?} bytes into: {:?}", byte_count, buf);
                self.reader.read_exact(&mut buf)?;
                Value::Large(BigInt::from_signed_bytes_be(&buf))
            };

            TagKind::Simple(tag.into(), value)
        } else {
            match byte.into() {
                tag @ ExtendedTag::ExtendedLiteral => {
                    let next_byte = self.reader.read_u8()?;
                    let is_small_value = 0 == (next_byte & 0b0000_1000);
                    let is_large_value = 0 == (next_byte & 0b0001_0000);
                    let short_value = next_byte >> 4;
                    let value = if is_small_value {
                        Value::Small(short_value.into())
                    } else if is_large_value {
                        let shift_value = (short_value as u32) << 3;
                        let next_byte = self.reader.read_u8()? as u32;
                        Value::Small(shift_value | next_byte)
                    } else {
                        panic!("We were expecting a small or small-extended value!");
                    };
                    TagKind::Extended(tag, value)
                }
                x => panic!("We don't know how handle {:?}", x),
            }
        };

        trace!("Read tagged value: {:?}", tagged_value);

        Ok(tagged_value.into())
    }
}

impl Into<u32> for CompactTerm {
    fn into(self) -> u32 {
        match self {
            CompactTerm::Literal(x) => x,
            CompactTerm::Atom(x) => x,
            CompactTerm::RegisterX(x) => x,
            CompactTerm::RegisterY(x) => x,
            CompactTerm::Label(x) => x,
            CompactTerm::Character(x) => x,
            CompactTerm::Integer(Value::Small(x)) => x,
            CompactTerm::ExtendedLiteral(Value::Small(x)) => x,
            _ => panic!("Cannot convert CompactTerm {:?} into u32 value", self),
        }
    }
}

impl Into<u8> for CompactTerm {
    fn into(self) -> u8 {
        match self {
            CompactTerm::Literal(x) => x as u8,
            CompactTerm::Atom(x) => x as u8,
            CompactTerm::RegisterX(x) => x as u8,
            CompactTerm::RegisterY(x) => x as u8,
            CompactTerm::Label(x) => x as u8,
            CompactTerm::Character(x) => x as u8,
            CompactTerm::Integer(Value::Small(x)) => x as u8,
            CompactTerm::ExtendedLiteral(Value::Small(x)) => x as u8,
            _ => panic!("Cannot convert CompactTerm {:?} into u8 value", self),
        }
    }
}
