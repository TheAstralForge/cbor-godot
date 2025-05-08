use godot::prelude::*;

struct GodotCbor;

#[gdextension]
unsafe impl ExtensionLibrary for GodotCbor {
    fn min_level() -> InitLevel {
        InitLevel::Core
    }
}

/// A CBOR encoder that can be used to encode data into a CBOR byte string.
#[derive(GodotClass)]
#[class(base = RefCounted, init)]
struct CborEncoder {
    base: Base<RefCounted>,

    /// The last error that occurred during encoding.
    last_error: Option<minicbor::encode::Error<std::convert::Infallible>>,

    /// The underlying buffer that we are writing to.
    #[init(val = minicbor::Encoder::new(PackedByteArrayWriter::default()))]
    encoder: minicbor::Encoder<PackedByteArrayWriter>,
}

#[godot_api]
impl CborEncoder {
    /// Returns the underlying packed byte array.
    ///
    /// Note that the complete buffer might not be written to yet. You can use
    /// the `size()` method to get the actual number of bytes written.
    ///
    /// Alternatively, you can use `finish()` to return the resized buffer.
    #[func]
    pub fn get_buffer(&self) -> PackedByteArray {
        self.encoder.writer().buffer.clone()
    }

    /// Returns the number of bytes written to the buffer.
    #[func]
    pub fn size(&self) -> u64 {
        self.encoder.writer().len as u64
    }

    /// Finishes the encoding process and returns the result as a packed byte array.
    ///
    /// This resets the encoder.
    #[func]
    pub fn finish(&mut self) -> PackedByteArray {
        std::mem::take(self.encoder.writer_mut()).into()
    }

    /// Returns a message describing the last error that occurred during encoding.
    #[func]
    pub fn error_message(&self) -> GString {
        match self.last_error.as_ref() {
            Some(err) => err.to_string().into(),
            None => GString::default(),
        }
    }

    /// Resets the encoder to its initial state.
    #[func]
    pub fn reset(&mut self) {
        self.encoder.writer_mut().len = 0;
    }

    /// Encodes an integer value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn int(&mut self, value: i64) -> bool {
        match self.encoder.i64(value) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes a floating point value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn float(&mut self, value: f64) -> bool {
        match self.encoder.f64(value) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes a boolean value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn bool(&mut self, value: bool) -> bool {
        match self.encoder.bool(value) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes a string value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn string(&mut self, value: String) -> bool {
        match self.encoder.str(&value) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes a string name value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn string_name(&mut self, value: StringName) -> bool {
        let s: String = value.into();
        match self.encoder.str(&s) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes a byte array value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn bytes(&mut self, value: PackedByteArray) -> bool {
        match self.encoder.bytes(value.as_slice()) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes a [`Variant`] value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the tag, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn encode(&mut self, value: Variant) -> bool {
        match self
            .encoder
            .encode_with(VariantWrapper(value), &mut EncodingContext::default())
        {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes a tag without an associated value.
    ///
    /// The caller is responsible for ensuring that a tag is added to the
    /// buffer after the tag is encoded.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the tag, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn tag(&mut self, tag: i64) -> bool {
        match self.encoder.tag(minicbor::data::Tag::new(tag as u64)) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Starts a new array in the encoder.
    ///
    /// The next `size` values will be encoded as part of the array.
    ///
    /// # Errors
    ///
    /// If an error occurs while starting the array, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn array(&mut self, size: u64) -> bool {
        match self.encoder.array(size) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Starts a new map in the encoder.
    ///
    /// The next `size` key-value pairs will be encoded as part of the map.
    ///
    /// # Errors
    ///
    /// If an error occurs while starting the map, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn map(&mut self, size: u64) -> bool {
        match self.encoder.map(size) {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Begins an indefinite array in the encoder.
    ///
    /// The array will continue until `end()` is called.
    ///
    /// # Errors
    ///
    /// If an error occurs while starting the array, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn begin_array(&mut self) -> bool {
        match self.encoder.begin_array() {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Begins an indefinite map in the encoder.
    ///
    /// The map will continue until `end()` is called.
    ///
    /// # Errors
    ///
    /// If an error occurs while starting the map, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn begin_map(&mut self) -> bool {
        match self.encoder.begin_map() {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Ends the current array or map in the encoder.
    ///
    /// This function should be called after all elements of the array or map
    /// have been added.
    ///
    /// # Errors
    ///
    /// If an error occurs while ending the array or map, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn end(&mut self) -> bool {
        match self.encoder.end() {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes the `null` value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the `null` value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func(rename = "nil")]
    pub fn null(&mut self) -> bool {
        match self.encoder.null() {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
    }

    /// Encodes the `undefined` value.
    ///
    /// # Errors
    ///
    /// If an error occurs while encoding the `undefined` value, the function returns
    /// `false` and the error can be accessed through the `error_message` method.
    #[func]
    pub fn undefined(&mut self) -> bool {
        match self.encoder.undefined() {
            Ok(_) => true,
            Err(err) => {
                self.last_error = Some(err);
                false
            }
        }
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
                godot_error!("Called `CBOR.error_message` when no error is present");
                GString::default()
            }
        }
    }

    /// Helper for encoding functions.
    fn _encode_into_helper<T: minicbor::Encode<EncodingContext>>(
        error_prefix: &str,
        writer: &mut PackedByteArrayWriter,
        obj: T,
    ) -> bool {
        match minicbor::encode_with(obj, writer, &mut EncodingContext::default()) {
            Ok(()) => true,
            Err(err) => {
                godot_error!("{error_prefix}: {err}");
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
        if Self::_encode_into_helper("CBOR.encode", &mut writer, VariantWrapper(variant)) {
            writer.into()
        } else {
            PackedByteArray::default()
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
                godot_error!("CBOR.decode_bytes: {err}");
                Variant::nil()
            }
        }
    }

    /// Prints the tokens of the provided [`PackedByteArray`] to the console.
    ///
    /// This can be used to debug an existing CBOR byte string.
    #[func]
    pub fn debug_bytes(data: PackedByteArray) -> String {
        minicbor::Decoder::new(data.as_slice()).tokens().to_string()
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
    encoder: Gd<CborEncoder>,
}

/// A wrapper around a godot [`Variant`] that implements the decoding trait.
struct VariantWrapper(pub Variant);

macro_rules! encode_err {
    ($msg:literal $(, $args:expr)* $(,)?) => {
        Err(minicbor::encode::Error::message(format_args!($msg $(, $args)*)))
    };
}

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
            VariantType::CALLABLE => {
                let callable = self.0.to::<Callable>();
                ctx.encoder.bind_mut().reset();
                let ok = callable.call(&[ctx.encoder.to_variant()]);
                if let Ok(msg) = ok.try_to::<String>() {
                    return Err(minicbor::encode::Error::message(msg));
                } else if !ok.is_nil() {
                    godot_warn!("Callables should return nothing or an error message");
                }
                e.writer_mut()
                    .write_all(ctx.encoder.bind().encoder.writer().as_slice())
                    .map_err(minicbor::encode::Error::write)?;
                Ok(())
            }
            VariantType::OBJECT => {
                let mut object = self.0.to::<Gd<Object>>();
                if object.has_method("_cbor_encode") {
                    ctx.encoder.bind_mut().reset();
                    match object.try_call("_cbor_encode", &[ctx.encoder.to_variant()]) {
                        Ok(ok) => {
                            if let Ok(msg) = ok.try_to::<String>() {
                                return Err(minicbor::encode::Error::message(msg));
                            } else if !ok.is_nil() {
                                godot_warn!(
                                    "`_encode_char` should return nothing or an error message"
                                );
                            }
                            e.writer_mut()
                                .write_all(ctx.encoder.bind().encoder.writer().as_slice())
                                .map_err(minicbor::encode::Error::write)?;
                            Ok(())
                        }
                        Err(err) => encode_err!(
                            "Failed to call `_cbor_encode(buf: PackedByteArray) -> void: {err}"
                        ),
                    }
                } else {
                    encode_err!(
                        "Object `{}` does not implement `_cbor_encode(buf: PackedByteArray) -> void`",
                        object.get_class()
                    )
                }
            }
            _ => encode_err!("Type {:?} cannot be encoded as CBOR", self.0.get_type()),
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

macro_rules! decode_err {
    ($msg:literal $(, $args:expr)* $(,)?) => {
        Err(minicbor::decode::Error::message(format_args!($msg $(, $args)*)))
    };
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
            Type::F16 => decode_err!("CBOR does not support half-precision floats",),
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
                decode_err!("unknown value received")
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

impl PackedByteArrayWriter {
    /// Returns a slice over the part of the buffer that has been written to.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        &self.buffer.as_slice()[..self.len]
    }
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
