#include "digestif.h"

#include "md5.h"
#include "sha1.h"
#include "sha256.h"
#include "sha512.h"
#include "blake2b.h"
#include "ripemd160.h"

#define __define_hash(name, upper)                                           \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ba_init (value ctx) {                           \
    digestif_ ## name ## _init ((struct name ## _ctx *) Caml_ba_data_val (ctx)); \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _st_init (value ctx) {                           \
    digestif_ ## name ## _init ((struct name ## _ctx *) Caml_ba_data_val (ctx)); \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ba_update (value ctx, value src, value off, value len) { \
    digestif_ ## name ## _update (                                           \
      (struct name ## _ctx *) Caml_ba_data_val (ctx),                        \
      _ba_uint8_off (src, off), Int_val (len));                              \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _st_update (value ctx, value src, value off, value len) { \
    digestif_ ## name ## _update (                                           \
      (struct name ## _ctx *) Caml_ba_data_val (ctx),                        \
      _st_uint8_off (src, off), Int_val (len));                              \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ba_finalize (value ctx, value dst, value off) { \
    digestif_ ## name ## _finalize (                                         \
      (struct name ## _ctx *) Caml_ba_data_val (ctx),                        \
      _ba_uint8_off (dst, off));                                             \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _st_finalize (value ctx, value dst, value off) { \
    digestif_ ## name ## _finalize(                                          \
      (struct name ## _ctx *) Caml_ba_data_val (ctx),                        \
      _st_uint8_off (dst, off));                                             \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ctx_size (__unit ()) {                          \
    return Val_int (upper ## _CTX_SIZE);                                     \
  }

__define_hash (md5, MD5)
__define_hash (sha1, SHA1)
__define_hash (sha224, SHA224)
__define_hash (sha256, SHA256)
__define_hash (sha384, SHA384)
__define_hash (sha512, SHA512)
__define_hash (blake2b, BLAKE2B)
__define_hash (rmd160, RMD160)

CAMLprim value
caml_digestif_blake2b_ba_abstract_init(value ctx, value outlen, value key, value off, value len)
{
  digestif_blake2b_abstract_init(
    (struct blake2b_ctx *) Caml_ba_data_val (ctx), Int_val (outlen),
    _ba_uint8_off(key, off), Int_val (len));

  return Val_unit;
}

CAMLprim value
caml_digestif_blake2b_st_abstract_init(value ctx, value outlen, value key, value off, value len)
{
  digestif_blake2b_abstract_init(
    (struct blake2b_ctx *) Caml_ba_data_val (ctx), Int_val (outlen),
    _st_uint8_off(key, off), Int_val (len));

  return Val_unit;
}

CAMLprim value
caml_digestif_blake2b_key_size(__unit ()) {
  return Val_int (BLAKE2B_KEYBYTES);
}

CAMLprim value
caml_digestif_blake2b_digest_size(value ctx) {
  return Val_int(((struct blake2b_ctx *) Caml_ba_data_val (ctx))->outlen);
}
