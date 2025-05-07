use godot::{global::push_error, prelude::*};

struct GodotCbor;

#[gdextension]
unsafe impl ExtensionLibrary for GodotCbor {
    fn min_level() -> InitLevel {
        InitLevel::Core
    }
}

#[derive(Debug, GodotClass)]
#[class(base = Resource, init, rename = CBOR)]
struct Cbor {
    base: Base<Resource>,
    error: Option<minicbor::decode::Error>,

    /// The result of parsing the CBOR Data.
    #[var]
    data: Variant,
}

#[godot_api]
impl Cbor {
    /// Returns whether the last call to [`decode`] has produced an error.
    #[func]
    pub fn has_error(&self) -> bool {
        self.error.is_some()
    }

    /// If an error is present, returns a message describing the error.
    #[func]
    pub fn error_message(&self) -> GString {
        match self.error.as_ref() {
            Some(err) => err.to_string().into(),
            None => {
                push_error(&[Variant::from(
                    "Called `CBOR.error_message` when no error is present",
                )]);
                GString::default()
            }
        }
    }

    /// Encodes the provided variant as a CBOR byte string.
    ///
    /// The string is pushed to the end of the provided [`PackedByteArray`].
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the variant to the provided
    /// array, the function returns `false` and logs the error.
    #[func]
    pub fn encode_into(variant: Variant, output: PackedByteArray) -> bool {
        let mut writer: PackedByteArrayWriter = output.into();
        match minicbor::encode(VariantWrapper(variant), &mut writer) {
            Ok(()) => true,
            Err(err) => {
                push_error(&[
                    Variant::from("CBOR.encode_into: "),
                    Variant::from(err.to_string()),
                ]);
                false
            }
        }
    }

    /// Encodes the provided variant as a CBOR byte string.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the variant,
    /// the returned [`PackedByteArray`] will be empty and the error
    /// will be logged.
    #[func]
    pub fn encode(variant: Variant) -> PackedByteArray {
        let mut writer = PackedByteArrayWriter::default();
        match minicbor::encode(VariantWrapper(variant), &mut writer) {
            Ok(()) => writer.into(),
            Err(err) => {
                push_error(&[
                    Variant::from("CBOR.decode_bytes: "),
                    Variant::from(err.to_string()),
                ]);
                PackedByteArray::new()
            }
        }
    }

    /// Decodes the provided [`PackedByteArray`] into a [`Variant`].
    ///
    /// # Errors
    ///
    /// Errors are logged and cause the function to produce a null value.
    #[func]
    pub fn decode_bytes(data: PackedByteArray) -> Variant {
        match minicbor::decode(data.as_slice()) {
            Ok(VariantWrapper(variant)) => variant,
            Err(err) => {
                push_error(&[
                    Variant::from("CBOR.decode_bytes: "),
                    Variant::from(err.to_string()),
                ]);
                Variant::nil()
            }
        }
    }

    /// Decodes the provided [`PackedByteArray`] into a [`Variant`].
    ///
    /// # Errors
    ///
    /// Errors are saved in the [`CBOR`] object and can be retrieved
    /// through associated methods.
    #[func]
    pub fn decode(&mut self, data: PackedByteArray) {
        match minicbor::decode(data.as_slice()) {
            Ok(VariantWrapper(variant)) => self.data = variant,
            Err(err) => self.error = Some(err),
        }
    }
}

/// A tagged value.
#[derive(Debug, GodotClass)]
#[class(base = RefCounted, init)]
struct Tagged {
    base: Base<RefCounted>,
    /// The semantic tag of the value.
    #[var]
    tag: i64,
    /// The value that is tagged.
    #[var]
    value: Variant,
}

/// A wrapper around a godot [`Variant`] that implements the decoding trait.
struct VariantWrapper(pub Variant);

impl<C> minicbor::encode::Encode<C> for VariantWrapper {
    fn encode<W: minicbor::encode::Write>(
        &self,
        e: &mut minicbor::Encoder<W>,
        ctx: &mut C,
    ) -> Result<(), minicbor::encode::Error<W::Error>> {
        match self.0.get_type() {
            VariantType::NIL => {
                e.null()?;
                Ok(())
            }
            VariantType::ARRAY => {
                let array = self.0.to::<VariantArray>();
                e.array(array.len() as u64)?;
                for item in array.iter_shared() {
                    e.encode_with(Self(item), ctx)?;
                }
                Ok(())
            }
            VariantType::BOOL => {
                e.bool(self.0.to::<bool>())?;
                Ok(())
            }
            VariantType::FLOAT => {
                e.f64(self.0.to::<f64>())?;
                Ok(())
            }
            VariantType::INT => {
                e.i64(self.0.to::<i64>())?;
                Ok(())
            }
            VariantType::DICTIONARY => {
                let dict = self.0.to::<Dictionary>();
                e.map(dict.len() as u64)?;
                for (key, value) in dict.iter_shared() {
                    e.encode_with(Self(key), ctx)?;
                    e.encode_with(Self(value), ctx)?;
                }
                Ok(())
            }
            VariantType::PACKED_BYTE_ARRAY => {
                let array = self.0.to::<PackedByteArray>();
                e.bytes(array.as_slice())?;
                Ok(())
            }
            VariantType::STRING | VariantType::STRING_NAME => {
                let string = self.0.to::<String>();
                e.str(&string)?;
                Ok(())
            }
            VariantType::PACKED_STRING_ARRAY => {
                let array = self.0.to::<PackedStringArray>();
                e.array(array.len() as u64)?;
                for string in array.as_slice() {
                    let s: String = string.into();
                    e.str(&s)?;
                }
                Ok(())
            }
            _ => Err(minicbor::encode::Error::message(format!(
                "Type {:?} cannot be encoded",
                self.0.get_type()
            ))),
        }
    }

    #[inline]
    fn is_nil(&self) -> bool {
        self.0.is_nil()
    }
}

impl<C> minicbor::decode::Decode<'_, C> for VariantWrapper {
    fn decode(d: &mut minicbor::Decoder<'_>, ctx: &mut C) -> Result<Self, minicbor::decode::Error> {
        use minicbor::data::Type;

        match d.datatype()? {
            Type::Array | Type::ArrayIndef => {
                let mut array = VariantArray::new();
                for item in d.array_iter_with::<_, Self>(ctx)? {
                    array.push(&item?.0);
                }
                Ok(Self(Variant::from(array)))
            }
            Type::Map | Type::MapIndef => {
                let mut dict = Dictionary::new();
                for item in d.map_iter_with::<_, Self, Self>(ctx)? {
                    let (key, value) = item?;
                    dict.set(key.0, value.0);
                }
                Ok(Self(Variant::from(dict)))
            }
            Type::Bool => {
                let value = d.bool()?;
                Ok(Self(Variant::from(value)))
            }
            Type::Bytes => {
                let bytes = d.bytes()?;
                Ok(Self(Variant::from(bytes)))
            }
            Type::BytesIndef => {
                let count = d
                    .probe()
                    .bytes_iter()?
                    .try_fold(0, |acc, x| x.map(|x| acc + x.len()))?;
                let mut array = PackedByteArray::new();
                array.resize(count);
                let mut cursor = 0;
                for item in d.bytes_iter()? {
                    let slice = item?;
                    array.as_mut_slice()[cursor..cursor + slice.len()].copy_from_slice(slice);
                    cursor += slice.len();
                }
                Ok(Self(Variant::from(array)))
            }
            Type::F16 => Err(minicbor::decode::Error::message(
                "received an unsupported half-precision floating-point number",
            )),
            Type::F32 => {
                let v = d.f32()?;
                Ok(Self(Variant::from(v)))
            }
            Type::F64 => {
                let v = d.f64()?;
                Ok(Self(Variant::from(v)))
            }
            Type::Int
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64 => {
                let v = d.i64()?;
                Ok(Self(Variant::from(v)))
            }
            Type::String => {
                let s = d.str()?;
                Ok(Self(Variant::from(s)))
            }
            Type::StringIndef => {
                let mut string = String::new();
                for item in d.str_iter()? {
                    let s = item?;
                    string.push_str(s);
                }
                Ok(Self(Variant::from(string)))
            }
            Type::Null => Ok(Self(Variant::nil())),
            Type::Undefined => Ok(Self(Variant::nil())),
            Type::Tag => {
                let tag = d.tag()?.as_u64() as i64;
                let value = d.decode_with::<C, Self>(ctx)?.0;
                Ok(Self(
                    Gd::from_init_fn(|base| Tagged { base, tag, value }).to_variant(),
                ))
            }
            Type::Simple | Type::Break | Type::Unknown(_) => {
                Err(minicbor::decode::Error::message("unknown value received"))
            }
        }
    }

    fn nil() -> Option<Self> {
        Some(Self(Variant::nil()))
    }
}

/// Allows writing directory to the memory managed by the Godot Engine instead
/// of copying stuff around.
#[derive(Default)]
pub struct PackedByteArrayWriter {
    /// The Godot-managed buffer.
    buffer: PackedByteArray,
    /// The number of bytes written to the buffer so far.
    len: usize,
}

impl From<PackedByteArray> for PackedByteArrayWriter {
    fn from(value: PackedByteArray) -> Self {
        let len = value.len();
        Self { buffer: value, len }
    }
}

impl From<PackedByteArrayWriter> for PackedByteArray {
    fn from(mut writer: PackedByteArrayWriter) -> Self {
        writer.buffer.resize(writer.len);
        writer.buffer
    }
}

impl minicbor::encode::Write for PackedByteArrayWriter {
    type Error = std::convert::Infallible;

    fn write_all(&mut self, buf: &[u8]) -> Result<(), Self::Error> {
        let remaining = self.buffer.len() - self.len;
        if remaining < buf.len() {
            let min_size = self
                .buffer
                .len()
                .checked_add(buf.len())
                .expect("capacity overflow");
            let amortized = self.buffer.len().saturating_mul(2);
            self.buffer.resize(amortized.max(min_size));
        }

        self.buffer.as_mut_slice()[self.len..self.len + buf.len()].copy_from_slice(buf);
        Ok(())
    }
}
