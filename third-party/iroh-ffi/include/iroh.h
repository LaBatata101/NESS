#ifndef IROH_FFI_H
#define IROH_FFI_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ABI version 1: synchronous facade used by the Zig binding. */

typedef struct IrohError IrohError;
typedef struct IrohEndpoint IrohEndpoint;
typedef struct IrohConnection IrohConnection;
typedef struct IrohSendStream IrohSendStream;
typedef struct IrohRecvStream IrohRecvStream;

typedef enum IrohStatus {
  IROH_STATUS_OK = 0,
  IROH_STATUS_ERROR = 1,
} IrohStatus;

typedef enum IrohPreset {
  IROH_PRESET_N0 = 0,
  IROH_PRESET_MINIMAL = 1,
  IROH_PRESET_N0_DISABLE_RELAY = 2,
} IrohPreset;

typedef enum IrohErrorKind {
  IROH_ERROR_INVALID_INPUT = 0,
  IROH_ERROR_BIND = 1,
  IROH_ERROR_CONNECT = 2,
  IROH_ERROR_CONNECTION = 3,
  IROH_ERROR_ALPN = 4,
  IROH_ERROR_KEY_PARSING = 5,
  IROH_ERROR_TICKET_PARSING = 6,
  IROH_ERROR_RELAY = 7,
  IROH_ERROR_STREAM = 8,
  IROH_ERROR_DATAGRAM = 9,
  IROH_ERROR_CALLBACK = 10,
  IROH_ERROR_CLOSED = 11,
  IROH_ERROR_TIMEOUT = 12,
  IROH_ERROR_INTERNAL = 13,
} IrohErrorKind;

typedef struct IrohByteSlice {
  const uint8_t *ptr;
  size_t len;
} IrohByteSlice;

/* Owned by the caller; release exactly once with iroh_owned_bytes_free. */
typedef struct IrohOwnedBytes {
  uint8_t *ptr;
  size_t len;
} IrohOwnedBytes;

typedef struct IrohEndpointOptions {
  uint32_t preset;
  /* Empty generates a fresh key; otherwise exactly 32 bytes. */
  IrohByteSlice secret_key;
  /* Empty uses the preset's default bind address. */
  IrohByteSlice bind_addr;
  const IrohByteSlice *alpns;
  size_t alpns_len;
} IrohEndpointOptions;

typedef enum IrohConnectionRoute {
  IROH_CONNECTION_ROUTE_UNKNOWN = 0,
  IROH_CONNECTION_ROUTE_DIRECT = 1,
  IROH_CONNECTION_ROUTE_RELAY = 2,
  IROH_CONNECTION_ROUTE_CUSTOM = 3,
} IrohConnectionRoute;

typedef struct IrohConnectionStats {
  /* If false, route and rtt_ms are unavailable. Aggregate counters are valid. */
  bool has_selected_path;
  IrohConnectionRoute route;
  uint64_t rtt_ms;
  uint64_t udp_tx_datagrams;
  uint64_t udp_tx_bytes;
  uint64_t udp_rx_datagrams;
  uint64_t udp_rx_bytes;
  uint64_t lost_packets;
  uint64_t lost_bytes;
} IrohConnectionStats;

uint32_t iroh_abi_version(void);
void iroh_owned_bytes_free(IrohOwnedBytes value);
void iroh_error_free(IrohError *error);
uint32_t iroh_error_kind(const IrohError *error);
IrohOwnedBytes iroh_error_message(const IrohError *error);

IrohStatus iroh_secret_key_generate(uint8_t (*out)[32], IrohError **out_error);
IrohStatus iroh_secret_key_public(IrohByteSlice secret, uint8_t (*out)[32],
                                  IrohError **out_error);
IrohStatus iroh_secret_key_sign(IrohByteSlice secret, IrohByteSlice message,
                                uint8_t (*out)[64], IrohError **out_error);
IrohStatus iroh_endpoint_id_verify(IrohByteSlice endpoint_id,
                                   IrohByteSlice message,
                                   IrohByteSlice signature,
                                   IrohError **out_error);
IrohStatus iroh_endpoint_id_from_string(IrohByteSlice value,
                                        uint8_t (*out)[32],
                                        IrohError **out_error);
IrohStatus iroh_endpoint_id_to_string(IrohByteSlice value,
                                      IrohOwnedBytes *out,
                                      IrohError **out_error);

/* Async iroh operations block the calling thread in this ABI. */
IrohStatus iroh_endpoint_bind(const IrohEndpointOptions *options,
                              IrohEndpoint **out, IrohError **out_error);
void iroh_endpoint_free(IrohEndpoint *endpoint);
IrohStatus iroh_endpoint_id(const IrohEndpoint *endpoint, uint8_t (*out)[32],
                            IrohError **out_error);
IrohStatus iroh_endpoint_secret_key(const IrohEndpoint *endpoint,
                                    uint8_t (*out)[32],
                                    IrohError **out_error);
IrohStatus iroh_endpoint_ticket(const IrohEndpoint *endpoint,
                                IrohOwnedBytes *out,
                                IrohError **out_error);
IrohStatus iroh_endpoint_connect_ticket(const IrohEndpoint *endpoint,
                                        IrohByteSlice ticket,
                                        IrohByteSlice alpn,
                                        IrohConnection **out,
                                        IrohError **out_error);
IrohStatus iroh_endpoint_accept(const IrohEndpoint *endpoint,
                                IrohConnection **out,
                                IrohError **out_error);
IrohStatus iroh_endpoint_close(const IrohEndpoint *endpoint,
                               IrohError **out_error);
bool iroh_endpoint_is_closed(const IrohEndpoint *endpoint);

void iroh_connection_free(IrohConnection *connection);
IrohStatus iroh_connection_alpn(const IrohConnection *connection,
                                IrohOwnedBytes *out,
                                IrohError **out_error);
IrohStatus iroh_connection_remote_id(const IrohConnection *connection,
                                     uint8_t (*out)[32],
                                     IrohError **out_error);
IrohStatus iroh_connection_stats(const IrohConnection *connection,
                                 IrohConnectionStats *out,
                                 IrohError **out_error);
IrohStatus iroh_connection_open_uni(const IrohConnection *connection,
                                    IrohSendStream **out,
                                    IrohError **out_error);
IrohStatus iroh_connection_accept_uni(const IrohConnection *connection,
                                      IrohRecvStream **out,
                                      IrohError **out_error);
IrohStatus iroh_connection_open_bi(const IrohConnection *connection,
                                   IrohSendStream **out_send,
                                   IrohRecvStream **out_recv,
                                   IrohError **out_error);
IrohStatus iroh_connection_accept_bi(const IrohConnection *connection,
                                     IrohSendStream **out_send,
                                     IrohRecvStream **out_recv,
                                     IrohError **out_error);
IrohStatus iroh_connection_send_datagram(const IrohConnection *connection,
                                         IrohByteSlice data,
                                         IrohError **out_error);
IrohStatus iroh_connection_read_datagram(const IrohConnection *connection,
                                         IrohOwnedBytes *out,
                                         IrohError **out_error);
IrohStatus iroh_connection_close(const IrohConnection *connection,
                                 uint64_t error_code, IrohByteSlice reason,
                                 IrohError **out_error);

void iroh_send_stream_free(IrohSendStream *stream);
IrohStatus iroh_send_stream_write_all(const IrohSendStream *stream,
                                      IrohByteSlice data,
                                      IrohError **out_error);
IrohStatus iroh_send_stream_finish(const IrohSendStream *stream,
                                   IrohError **out_error);
IrohStatus iroh_send_stream_reset(const IrohSendStream *stream,
                                  uint64_t error_code,
                                  IrohError **out_error);

void iroh_recv_stream_free(IrohRecvStream *stream);
IrohStatus iroh_recv_stream_read(const IrohRecvStream *stream,
                                 uint32_t size_limit, IrohOwnedBytes *out,
                                 IrohError **out_error);
IrohStatus iroh_recv_stream_read_exact(const IrohRecvStream *stream,
                                       uint32_t size, IrohOwnedBytes *out,
                                       IrohError **out_error);
IrohStatus iroh_recv_stream_read_to_end(const IrohRecvStream *stream,
                                        uint32_t size_limit,
                                        IrohOwnedBytes *out,
                                        IrohError **out_error);
IrohStatus iroh_recv_stream_stop(const IrohRecvStream *stream,
                                 uint64_t error_code,
                                 IrohError **out_error);

#ifdef __cplusplus
}
#endif

#endif /* IROH_FFI_H */
