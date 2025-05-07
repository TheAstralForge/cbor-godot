use godot::{
    global::{push_error, push_warning},
    prelude::*,
};

struct GodotCbor;

#[gdextension]
unsafe impl ExtensionLibrary for GodotCbor {
    fn min_level() -> InitLevel {
        InitLevel::Core
    }
}

/// Contains a collection of callback that hook into the default decoding process
/// to allow for custom decoding of objects.
#[derive(Debug, GodotClass)]
#[class(base = RefCounted, init)]
struct CborDecodeReplacer {
    base: Base<RefCounted>,
}

#[godot_api]
impl CborDecodeReplacer {
    /// Replaces the provided [`Array`] with something else.
    #[func(virtual)]
    pub fn replace_array(&self, array: VariantArray) -> Variant {
        array.to_variant()
    }

    /// Replaces the provided [`Dictionary`] with something else.
    #[func(virtual)]
    pub fn replace_dictionary(&self, dict: Dictionary) -> Variant {
        dict.to_variant()
    }

    /// Replaces the provided boolean with something else.
    #[func(virtual)]
    pub fn replace_bool(&self, value: bool) -> Variant {
        value.to_variant()
    }

    /// Replaces the provided [`PackedByteArray`] with something else.
    #[func(virtual)]
    pub fn replace_packed_byte_array(&self, array: PackedByteArray) -> Variant {
        array.to_variant()
    }

    /// Replaces the provided floating point number with something else.
    #[func(virtual)]
    pub fn replace_float(&self, value: f64) -> Variant {
        value.to_variant()
    }

    /// Replaces the provided [`String`] with something else.
    #[func(virtual)]
    pub fn replace_string(&self, string: String) -> Variant {
        string.to_variant()
    }

    /// Replaces the provided [`PackedByteArray`] with something else.
    #[func(virtual)]
    pub fn replace_packed_string_array(&self, array: PackedStringArray) -> Variant {
        array.to_variant()
    }

    /// Replaces the provided tagged value with something else.
    #[func(virtual)]
    pub fn replace_tagged(&self, tag: i64, value: Variant) -> Variant {
        _ = tag;
        value.to_variant()
    }
}

#[derive(Debug, GodotClass)]
#[class(base = Resource, init, rename = CBOR)]
struct Cbor {
    base: Base<Resource>,
    error: Option<minicbor::decode::Error>,

    /// The replacer instance that will be used to replace the decoded values.
    #[var]
    replacer: Gd<CborDecodeReplacer>,
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
        match minicbor::encode_with(
            VariantWrapper(variant),
            &mut writer,
            &mut EncodingContext::default(),
        ) {
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
        match minicbor::encode_with(
            VariantWrapper(variant),
            &mut writer,
            &mut EncodingContext::default(),
        ) {
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
        match minicbor::decode_with(data.as_slice(), &mut {
            DecodingContext {
                replacer: CborDecodeReplacer::new_gd(),
            }
        }) {
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

    /// Prints the tokens of the provided [`PackedByteArray`] to the console.
    ///
    /// This can be used to debug an existing CBOR byte string.
    #[func]
    pub fn debug_decode_bytes(data: PackedByteArray) {
        let mut decoder = minicbor::Decoder::new(data.as_slice());
        let mut stack: Vec<u64> = Vec::new();
        godot_print!("Tokens:");
        for result in decoder.tokens() {
            use minicbor::data::Token;

            match result {
                Ok(token) => {
                    godot_print!("{: >2$} - {:?}", "", token, stack.len() * 2);

                    stack.pop_if(|n| {
                        if *n == u64::MAX {
                            return false;
                        }

                        *n = n.saturating_sub(1);
                        *n == 0
                    });

                    match token {
                        Token::Array(n) => {
                            stack.push(n);
                        }
                        Token::BeginArray => {
                            stack.push(u64::MAX);
                        }
                        Token::Break => {
                            stack.pop();
                        }
                        Token::Map(n) => {
                            stack.push(n + 2);
                        }
                        Token::BeginMap => {
                            stack.push(u64::MAX);
                        }
                        _ => {}
                    }
                }
                Err(err) => {
                    godot_print!("Error: {:?}", err);
                    break;
                }
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
        match minicbor::decode_with(
            data.as_slice(),
            &mut DecodingContext {
                replacer: self.replacer.clone(),
            },
        ) {
            Ok(VariantWrapper(variant)) => self.data = variant,
            Err(err) => self.error = Some(err),
        }
    }
}

/// The context used to encode data.
#[derive(Default)]
pub struct EncodingContext {
    /// The array that will be passed to `_cbor_encode` methods.
    buffer: PackedByteArray,
}

/// A wrapper around a godot [`Variant`] that implements the decoding trait.
struct VariantWrapper(pub Variant);

impl minicbor::encode::Encode<EncodingContext> for VariantWrapper {
    fn encode<W: minicbor::encode::Write>(
        &self,
        e: &mut minicbor::Encoder<W>,
        ctx: &mut EncodingContext,
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
            VariantType::STRING => {
                let string = self.0.to::<String>();
                e.str(&string)?;
                Ok(())
            }
            VariantType::STRING_NAME => {
                let s = self.0.to::<StringName>();
                let s: String = s.into();
                e.str(&s)?;
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
            VariantType::OBJECT => {
                let mut object = self.0.to::<Gd<Object>>();
                if object.has_method("_cbor_encode") {
                    ctx.buffer.clear();
                    match object.try_call("_cbor_encode", &[ctx.buffer.to_variant()]) {
                        Ok(ok) => {
                            if !ok.is_nil() {
                                push_warning(&[Variant::from(
                                    "`_encode_char` should return nothing",
                                )]);
                            }
                            Ok(())
                        }
                        Err(err) => Err(minicbor::encode::Error::message(format!(
                            "Failed to call `_cbor_encode(buf: PackedByteArray) -> void: {err}",
                        ))),
                    }
                } else if object.has_method("_cbor_replace") {
                    match object.try_call("_cbor_replace", &[]) {
                        Ok(ok) => {
                            e.encode_with(Self(ok), ctx)?;
                            Ok(())
                        }
                        Err(err) => Err(minicbor::encode::Error::message(format!(
                            "Failed to call `_cbor_replace() -> Variant: {err}",
                        ))),
                    }
                } else {
                    Err(minicbor::encode::Error::message(format!(
                        "Object `{}` does not implement `_cbor_encode(buf: PackedByteArray) -> void`",
                        object.get_class()
                    )))
                }
            }
            _ => Err(minicbor::encode::Error::message(format!(
                "Type {:?} cannot be encoded as CBOR",
                self.0.get_type()
            ))),
        }
    }

    #[inline]
    fn is_nil(&self) -> bool {
        self.0.is_nil()
    }
}

/// The context that is passed to the decoding process.
pub struct DecodingContext {
    /// The decode replacer that will be used to replace the decoded values.
    replacer: Gd<CborDecodeReplacer>,
}

impl minicbor::decode::Decode<'_, DecodingContext> for VariantWrapper {
    fn decode(
        d: &mut minicbor::Decoder<'_>,
        ctx: &mut DecodingContext,
    ) -> Result<Self, minicbor::decode::Error> {
        use minicbor::data::Type;

        match d.datatype()? {
            Type::Array | Type::ArrayIndef => {
                let mut array = VariantArray::new();
                for item in d.array_iter_with::<_, Self>(ctx)? {
                    array.push(&item?.0);
                }
                Ok(Self(ctx.replacer.bind().replace_array(array)))
            }
            Type::Map | Type::MapIndef => {
                let mut dict = Dictionary::new();
                for item in d.map_iter_with::<_, Self, Self>(ctx)? {
                    let (key, value) = item?;
                    dict.set(key.0, value.0);
                }
                Ok(Self(ctx.replacer.bind().replace_dictionary(dict)))
            }
            Type::Bool => {
                let value = d.bool()?;
                Ok(Self(ctx.replacer.bind().replace_bool(value)))
            }
            Type::Bytes => {
                let bytes = d.bytes()?;
                let mut array = PackedByteArray::new();
                array.resize(bytes.len());
                array.as_mut_slice().copy_from_slice(bytes);
                Ok(Self(ctx.replacer.bind().replace_packed_byte_array(array)))
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
                Ok(Self(ctx.replacer.bind().replace_packed_byte_array(array)))
            }
            Type::F16 => {
                let v = d.f16()?;
                Ok(Self(ctx.replacer.bind().replace_float(v as f64)))
            }
            Type::F32 => {
                let v = d.f32()?;
                Ok(Self(ctx.replacer.bind().replace_float(v as f64)))
            }
            Type::F64 => {
                let v = d.f64()?;
                Ok(Self(ctx.replacer.bind().replace_float(v)))
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
                let value = d.decode_with::<_, Self>(ctx)?.0;
                Ok(Self(ctx.replacer.bind().replace_tagged(tag, value)))
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
        self.len += buf.len();
        Ok(())
    }
}
