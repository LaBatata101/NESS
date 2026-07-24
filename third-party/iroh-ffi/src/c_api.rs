//! Stable C ABI used by the Zig binding.
//!
//! UniFFI's exported ABI is an implementation detail of its generated bindings.
//! Zig therefore uses this small, explicit facade instead.  Every operation
//! that can fail returns an [`IrohStatus`] and writes an owned [`IrohError`] to
//! `out_error`.  Async iroh operations are driven on a process-wide Tokio
//! runtime and block the calling thread.

use std::{
    any::Any,
    panic::{AssertUnwindSafe, catch_unwind},
    ptr, slice, str,
    sync::OnceLock,
};

use crate::{
    BiStream, Connection, Endpoint, EndpointId, EndpointOptions, EndpointTicket, IrohError,
    IrohErrorKind, RecvStream, SecretKey, SendStream, Signature, preset_minimal, preset_n0,
    preset_n0_disable_relay,
};

/// Increment this when an incompatible change is made to `include/iroh.h`.
const ABI_VERSION: u32 = 1;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IrohStatus {
    Ok = 0,
    Error = 1,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct IrohByteSlice {
    pub ptr: *const u8,
    pub len: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct IrohOwnedBytes {
    pub ptr: *mut u8,
    pub len: usize,
}

impl IrohOwnedBytes {
    fn from_vec(value: Vec<u8>) -> Self {
        if value.is_empty() {
            return Self {
                ptr: ptr::null_mut(),
                len: 0,
            };
        }
        let boxed = value.into_boxed_slice();
        let len = boxed.len();
        let ptr = Box::into_raw(boxed).cast::<u8>();
        Self { ptr, len }
    }
}

#[repr(C)]
pub struct IrohEndpointOptions {
    pub preset: u32,
    /// Empty means generate a new key; otherwise this must contain 32 bytes.
    pub secret_key: IrohByteSlice,
    /// Empty means use the preset's default bind address.
    pub bind_addr: IrohByteSlice,
    pub alpns: *const IrohByteSlice,
    pub alpns_len: usize,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IrohConnectionRoute {
    Unknown = 0,
    Direct = 1,
    Relay = 2,
    Custom = 3,
}

/// Fixed-size snapshot of the selected path and aggregate QUIC connection
/// counters. A false `has_selected_path` means that `route` and `rtt_ms` are
/// not currently available; the aggregate counters remain valid.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct IrohConnectionStats {
    pub has_selected_path: bool,
    pub route: IrohConnectionRoute,
    pub rtt_ms: u64,
    pub udp_tx_datagrams: u64,
    pub udp_tx_bytes: u64,
    pub udp_rx_datagrams: u64,
    pub udp_rx_bytes: u64,
    pub lost_packets: u64,
    pub lost_bytes: u64,
}

static RUNTIME: OnceLock<Result<tokio::runtime::Runtime, String>> = OnceLock::new();

fn runtime() -> Result<&'static tokio::runtime::Runtime, IrohError> {
    RUNTIME
        .get_or_init(|| {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()
                .map_err(|error| error.to_string())
        })
        .as_ref()
        .map_err(|message| {
            IrohError::new(
                IrohErrorKind::Internal,
                format!("failed to initialize Tokio runtime: {message}"),
                message.clone(),
            )
        })
}

fn panic_error(payload: Box<dyn Any + Send>) -> IrohError {
    let message = if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_owned()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "Rust panic crossed the iroh C ABI".to_owned()
    };
    IrohError::new(IrohErrorKind::Internal, message.clone(), message)
}

unsafe fn put_error(out_error: *mut *mut IrohError, error: IrohError) {
    if !out_error.is_null() {
        // SAFETY: The caller promises that a non-null out parameter is writable.
        unsafe { out_error.write(Box::into_raw(Box::new(error))) };
    }
}

unsafe fn ffi_call<T>(
    out: *mut T,
    out_error: *mut *mut IrohError,
    call: impl FnOnce() -> Result<T, IrohError>,
) -> IrohStatus {
    if !out_error.is_null() {
        // SAFETY: The caller promises that a non-null out parameter is writable.
        unsafe { out_error.write(ptr::null_mut()) };
    }
    if out.is_null() {
        unsafe { put_error(out_error, IrohError::invalid_input("out must not be null")) };
        return IrohStatus::Error;
    }
    match catch_unwind(AssertUnwindSafe(call)) {
        Ok(Ok(value)) => {
            // SAFETY: Null was rejected above and the caller promises writability.
            unsafe { out.write(value) };
            IrohStatus::Ok
        }
        Ok(Err(error)) => {
            unsafe { put_error(out_error, error) };
            IrohStatus::Error
        }
        Err(payload) => {
            unsafe { put_error(out_error, panic_error(payload)) };
            IrohStatus::Error
        }
    }
}

unsafe fn ffi_unit(
    out_error: *mut *mut IrohError,
    call: impl FnOnce() -> Result<(), IrohError>,
) -> IrohStatus {
    let mut unit = ();
    unsafe { ffi_call(&mut unit, out_error, call) }
}

unsafe fn bytes<'a>(value: IrohByteSlice, name: &str) -> Result<&'a [u8], IrohError> {
    if value.len == 0 {
        return Ok(&[]);
    }
    if value.ptr.is_null() {
        return Err(IrohError::invalid_input(format!(
            "{name}.ptr must not be null when len is non-zero"
        )));
    }
    // SAFETY: The caller promises the range is readable for the duration of the call.
    Ok(unsafe { slice::from_raw_parts(value.ptr, value.len) })
}

unsafe fn utf8(value: IrohByteSlice, name: &str) -> Result<String, IrohError> {
    let value = unsafe { bytes(value, name) }?;
    str::from_utf8(value)
        .map(str::to_owned)
        .map_err(|error| IrohError::invalid_input(format!("{name} is not UTF-8: {error}")))
}

unsafe fn reference<'a, T>(value: *const T, name: &str) -> Result<&'a T, IrohError> {
    // SAFETY: The caller owns a live handle until this call returns.
    unsafe { value.as_ref() }
        .ok_or_else(|| IrohError::invalid_input(format!("{name} must not be null")))
}

fn error_kind_value(kind: IrohErrorKind) -> u32 {
    match kind {
        IrohErrorKind::InvalidInput => 0,
        IrohErrorKind::Bind => 1,
        IrohErrorKind::Connect => 2,
        IrohErrorKind::Connection => 3,
        IrohErrorKind::Alpn => 4,
        IrohErrorKind::KeyParsing => 5,
        IrohErrorKind::TicketParsing => 6,
        IrohErrorKind::Relay => 7,
        IrohErrorKind::Stream => 8,
        IrohErrorKind::Datagram => 9,
        IrohErrorKind::Callback => 10,
        IrohErrorKind::Closed => 11,
        IrohErrorKind::Timeout => 12,
        IrohErrorKind::Internal => 13,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn iroh_abi_version() -> u32 {
    ABI_VERSION
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_owned_bytes_free(value: IrohOwnedBytes) {
    if value.len == 0 {
        return;
    }
    if !value.ptr.is_null() {
        let raw = ptr::slice_from_raw_parts_mut(value.ptr, value.len);
        // SAFETY: `IrohOwnedBytes` values are created from boxed slices above and
        // ownership is transferred to the caller exactly once.
        drop(unsafe { Box::from_raw(raw) });
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_error_free(error: *mut IrohError) {
    if !error.is_null() {
        // SAFETY: The handle was returned by this module and is freed once.
        drop(unsafe { Box::from_raw(error) });
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_error_kind(error: *const IrohError) -> u32 {
    // Invalid handles are caller UB; return Internal for a defensive null check.
    unsafe { error.as_ref() }
        .map(|error| error_kind_value(error.kind()))
        .unwrap_or(13)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_error_message(error: *const IrohError) -> IrohOwnedBytes {
    unsafe { error.as_ref() }
        .map(|error| IrohOwnedBytes::from_vec(error.message().into_bytes()))
        .unwrap_or_else(|| IrohOwnedBytes::from_vec(Vec::new()))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_secret_key_generate(
    out: *mut [u8; 32],
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            Ok(SecretKey::generate().to_bytes().try_into().unwrap())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_secret_key_public(
    secret: IrohByteSlice,
    out: *mut [u8; 32],
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let secret = SecretKey::from_bytes(bytes(secret, "secret")?.to_vec())?;
            Ok(secret.public().to_bytes().try_into().unwrap())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_secret_key_sign(
    secret: IrohByteSlice,
    message: IrohByteSlice,
    out: *mut [u8; 64],
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let secret = SecretKey::from_bytes(bytes(secret, "secret")?.to_vec())?;
            let signature = secret.sign(bytes(message, "message")?);
            Ok(signature.to_bytes().try_into().unwrap())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_id_verify(
    endpoint_id: IrohByteSlice,
    message: IrohByteSlice,
    signature: IrohByteSlice,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            let id = EndpointId::from_bytes(bytes(endpoint_id, "endpoint_id")?.to_vec())?;
            let signature = Signature::from_bytes(bytes(signature, "signature")?.to_vec())?;
            id.verify(bytes(message, "message")?, &signature)
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_id_from_string(
    value: IrohByteSlice,
    out: *mut [u8; 32],
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let id = EndpointId::from_string(utf8(value, "endpoint_id")?)?;
            Ok(id.to_bytes().try_into().unwrap())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_id_to_string(
    value: IrohByteSlice,
    out: *mut IrohOwnedBytes,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let id = EndpointId::from_bytes(bytes(value, "endpoint_id")?.to_vec())?;
            Ok(IrohOwnedBytes::from_vec(id.to_string().into_bytes()))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_bind(
    options: *const IrohEndpointOptions,
    out: *mut *mut Endpoint,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let options = reference(options, "options")?;
            let secret_key = bytes(options.secret_key, "secret_key")?;
            let bind_addr = utf8(options.bind_addr, "bind_addr")?;
            let alpns = if options.alpns_len == 0 {
                &[][..]
            } else {
                if options.alpns.is_null() {
                    return Err(IrohError::invalid_input(
                        "alpns must not be null when alpns_len is non-zero",
                    ));
                }
                slice::from_raw_parts(options.alpns, options.alpns_len)
            };
            let alpns = alpns
                .iter()
                .enumerate()
                .map(|(index, value)| bytes(*value, &format!("alpns[{index}]")).map(Vec::from))
                .collect::<Result<Vec<_>, _>>()?;
            let preset = match options.preset {
                0 => preset_n0(),
                1 => preset_minimal(),
                2 => preset_n0_disable_relay(),
                value => {
                    return Err(IrohError::invalid_input(format!(
                        "unknown endpoint preset: {value}"
                    )));
                }
            };
            let options = EndpointOptions {
                preset: Some(preset),
                bind_addr: (!bind_addr.is_empty()).then_some(bind_addr),
                secret_key: (!secret_key.is_empty()).then(|| secret_key.to_vec()),
                alpns: (!alpns.is_empty()).then_some(alpns),
                relay_mode: None,
                protocols: None,
            };
            let endpoint = runtime()?.block_on(Endpoint::bind(options))?;
            Ok(Box::into_raw(Box::new(endpoint)))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_free(endpoint: *mut Endpoint) {
    if !endpoint.is_null() {
        drop(unsafe { Box::from_raw(endpoint) });
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_id(
    endpoint: *const Endpoint,
    out: *mut [u8; 32],
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            Ok(reference(endpoint, "endpoint")?
                .id()
                .to_bytes()
                .try_into()
                .unwrap())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_secret_key(
    endpoint: *const Endpoint,
    out: *mut [u8; 32],
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            Ok(reference(endpoint, "endpoint")?
                .secret_key()
                .to_bytes()
                .try_into()
                .unwrap())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_ticket(
    endpoint: *const Endpoint,
    out: *mut IrohOwnedBytes,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let endpoint = reference(endpoint, "endpoint")?;
            let ticket = EndpointTicket::from_addr(&endpoint.addr())?;
            Ok(IrohOwnedBytes::from_vec(ticket.to_string().into_bytes()))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_connect_ticket(
    endpoint: *const Endpoint,
    ticket: IrohByteSlice,
    alpn: IrohByteSlice,
    out: *mut *mut Connection,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let endpoint = reference(endpoint, "endpoint")?;
            let ticket = EndpointTicket::from_string(utf8(ticket, "ticket")?)?;
            let alpn = bytes(alpn, "alpn")?.to_vec();
            let connection =
                runtime()?.block_on(endpoint.connect(&ticket.endpoint_addr(), &alpn))?;
            Ok(Box::into_raw(Box::new(connection)))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_accept(
    endpoint: *const Endpoint,
    out: *mut *mut Connection,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let endpoint = reference(endpoint, "endpoint")?;
            let connection = runtime()?.block_on(async {
                let incoming = endpoint.accept_next().await.ok_or_else(|| {
                    IrohError::new(
                        IrohErrorKind::Closed,
                        "endpoint is closed",
                        "endpoint is closed",
                    )
                })?;
                incoming.accept().await?.connect().await
            })?;
            Ok(Box::into_raw(Box::new(connection)))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_close(
    endpoint: *const Endpoint,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            runtime()?.block_on(reference(endpoint, "endpoint")?.close())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_endpoint_is_closed(endpoint: *const Endpoint) -> bool {
    unsafe { endpoint.as_ref() }
        .map(Endpoint::is_closed)
        .unwrap_or(true)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_free(connection: *mut Connection) {
    if !connection.is_null() {
        drop(unsafe { Box::from_raw(connection) });
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_alpn(
    connection: *const Connection,
    out: *mut IrohOwnedBytes,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            Ok(IrohOwnedBytes::from_vec(
                reference(connection, "connection")?.alpn(),
            ))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_remote_id(
    connection: *const Connection,
    out: *mut [u8; 32],
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            Ok(reference(connection, "connection")?
                .remote_id()
                .to_bytes()
                .try_into()
                .unwrap())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_stats(
    connection: *const Connection,
    out: *mut IrohConnectionStats,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            const fn counter(value: i64) -> u64 {
                if value < 0 { 0 } else { value as u64 }
            }

            let connection = reference(connection, "connection")?;
            let aggregate = connection.stats();
            let paths = connection.paths();
            let selected = paths.iter().find(|path| path.is_selected);
            let route = selected.map_or(IrohConnectionRoute::Unknown, |path| {
                if path.is_ip {
                    IrohConnectionRoute::Direct
                } else if path.is_relay {
                    IrohConnectionRoute::Relay
                } else {
                    IrohConnectionRoute::Custom
                }
            });

            Ok(IrohConnectionStats {
                has_selected_path: selected.is_some(),
                route,
                rtt_ms: selected.map_or(0, |path| path.rtt_ms),
                udp_tx_datagrams: counter(aggregate.udp_tx_datagrams),
                udp_tx_bytes: counter(aggregate.udp_tx_bytes),
                udp_rx_datagrams: counter(aggregate.udp_rx_datagrams),
                udp_rx_bytes: counter(aggregate.udp_rx_bytes),
                lost_packets: counter(aggregate.lost_packets),
                lost_bytes: counter(aggregate.lost_bytes),
            })
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_open_uni(
    connection: *const Connection,
    out: *mut *mut SendStream,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let stream = runtime()?.block_on(reference(connection, "connection")?.open_uni())?;
            Ok(Box::into_raw(Box::new(stream)))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_accept_uni(
    connection: *const Connection,
    out: *mut *mut RecvStream,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let stream = runtime()?.block_on(reference(connection, "connection")?.accept_uni())?;
            Ok(Box::into_raw(Box::new(stream)))
        })
    }
}

unsafe fn bi_stream(
    connection: *const Connection,
    accept: bool,
    out_send: *mut *mut SendStream,
    out_recv: *mut *mut RecvStream,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            if out_send.is_null() || out_recv.is_null() {
                return Err(IrohError::invalid_input(
                    "out_send and out_recv must not be null",
                ));
            }
            let connection = reference(connection, "connection")?;
            let stream: BiStream = if accept {
                runtime()?.block_on(connection.accept_bi())?
            } else {
                runtime()?.block_on(connection.open_bi())?
            };
            out_send.write(Box::into_raw(Box::new(stream.send())));
            out_recv.write(Box::into_raw(Box::new(stream.recv())));
            Ok(())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_open_bi(
    connection: *const Connection,
    out_send: *mut *mut SendStream,
    out_recv: *mut *mut RecvStream,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe { bi_stream(connection, false, out_send, out_recv, out_error) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_accept_bi(
    connection: *const Connection,
    out_send: *mut *mut SendStream,
    out_recv: *mut *mut RecvStream,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe { bi_stream(connection, true, out_send, out_recv, out_error) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_send_datagram(
    connection: *const Connection,
    data: IrohByteSlice,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            reference(connection, "connection")?.send_datagram(bytes(data, "data")?.to_vec())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_read_datagram(
    connection: *const Connection,
    out: *mut IrohOwnedBytes,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let data = runtime()?.block_on(reference(connection, "connection")?.read_datagram())?;
            Ok(IrohOwnedBytes::from_vec(data))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_connection_close(
    connection: *const Connection,
    error_code: u64,
    reason: IrohByteSlice,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            let error_code = i64::try_from(error_code)
                .map_err(|_| IrohError::invalid_input("error_code must fit in i64"))?;
            reference(connection, "connection")?.close(error_code, bytes(reason, "reason")?)
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_send_stream_free(stream: *mut SendStream) {
    if !stream.is_null() {
        drop(unsafe { Box::from_raw(stream) });
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_send_stream_write_all(
    stream: *const SendStream,
    data: IrohByteSlice,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            runtime()?.block_on(reference(stream, "send_stream")?.write_all(bytes(data, "data")?))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_send_stream_finish(
    stream: *const SendStream,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            runtime()?.block_on(reference(stream, "send_stream")?.finish())
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_send_stream_reset(
    stream: *const SendStream,
    error_code: u64,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            runtime()?.block_on(reference(stream, "send_stream")?.reset(error_code))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_recv_stream_free(stream: *mut RecvStream) {
    if !stream.is_null() {
        drop(unsafe { Box::from_raw(stream) });
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_recv_stream_read(
    stream: *const RecvStream,
    size_limit: u32,
    out: *mut IrohOwnedBytes,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let data = runtime()?.block_on(reference(stream, "recv_stream")?.read(size_limit))?;
            Ok(IrohOwnedBytes::from_vec(data))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_recv_stream_read_exact(
    stream: *const RecvStream,
    size: u32,
    out: *mut IrohOwnedBytes,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let data = runtime()?.block_on(reference(stream, "recv_stream")?.read_exact(size))?;
            Ok(IrohOwnedBytes::from_vec(data))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_recv_stream_read_to_end(
    stream: *const RecvStream,
    size_limit: u32,
    out: *mut IrohOwnedBytes,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_call(out, out_error, || {
            let data =
                runtime()?.block_on(reference(stream, "recv_stream")?.read_to_end(size_limit))?;
            Ok(IrohOwnedBytes::from_vec(data))
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn iroh_recv_stream_stop(
    stream: *const RecvStream,
    error_code: u64,
    out_error: *mut *mut IrohError,
) -> IrohStatus {
    unsafe {
        ffi_unit(out_error, || {
            runtime()?.block_on(reference(stream, "recv_stream")?.stop(error_code))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn c_crypto_roundtrip() {
        unsafe {
            let mut error = ptr::null_mut();
            let mut secret = [0; 32];
            assert_eq!(
                iroh_secret_key_generate(&mut secret, &mut error),
                IrohStatus::Ok
            );
            assert!(error.is_null());

            let mut public = [0; 32];
            assert_eq!(
                iroh_secret_key_public(
                    IrohByteSlice {
                        ptr: secret.as_ptr(),
                        len: secret.len(),
                    },
                    &mut public,
                    &mut error,
                ),
                IrohStatus::Ok
            );

            let message = b"hello from C";
            let mut signature = [0; 64];
            assert_eq!(
                iroh_secret_key_sign(
                    IrohByteSlice {
                        ptr: secret.as_ptr(),
                        len: secret.len(),
                    },
                    IrohByteSlice {
                        ptr: message.as_ptr(),
                        len: message.len(),
                    },
                    &mut signature,
                    &mut error,
                ),
                IrohStatus::Ok
            );
            assert_eq!(
                iroh_endpoint_id_verify(
                    IrohByteSlice {
                        ptr: public.as_ptr(),
                        len: public.len(),
                    },
                    IrohByteSlice {
                        ptr: message.as_ptr(),
                        len: message.len(),
                    },
                    IrohByteSlice {
                        ptr: signature.as_ptr(),
                        len: signature.len(),
                    },
                    &mut error,
                ),
                IrohStatus::Ok
            );
        }
    }
}
