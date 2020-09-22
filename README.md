# SDP foreign extension

`sdp-foreign` is an extension for interoperability with other languages.

## Motivation

Previously, the only way (direct, in one action) to serialize/deserialize `sdp`
structures by pointer was `sdp-ctypes` extension. Now `sdp` provides `Thaw`
instances for serializing immutable structures, and most of the `sdp-ctypes` are
included in the library.

`sdp-foreign` provides another option for working with foreign data -
sdp-compatible structures based on pointers: foreign linked lists and pointer
array.

## Alternatives

`sdp-foreign` is designed to work with `FFI` and doesn't provide any guarantees
of security or correctness. To work with binary streams (files, sockets, pipes
and other `ByteString`-like data) use `sdp-binary`.

## Versioning

`sdp-foreign` follows of the [Haskell PVP](https://pvp.haskell.org) and `sdp`
extension rules.

