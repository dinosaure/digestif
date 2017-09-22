module Bi         = Digestif_bigstring
module By         = Digestif_bytes
module Pp         = Digestif_pp
module Xor        = Baijiu_xor

module type S =
sig
  val digest_size : int

  module Bigstring :
  sig
    type buffer = Bi.t
    type ctx
    type t = Bi.t

    val init        : unit -> ctx
    val feed        : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bi.t -> unit
    val get         : ctx -> t

    val digest      : buffer -> t
    val digestv     : buffer list -> t
    val hmac        : key:buffer -> buffer -> t
    val hmacv       : key:buffer -> buffer list -> t

    val compare     : t -> t -> int
    val eq          : t -> t -> bool
    val neq         : t -> t -> bool

    val pp          : Format.formatter -> t -> unit
    val of_hex      : buffer -> t
    val to_hex      : t -> buffer
  end

  module Bytes :
  sig
    type buffer = By.t
    type ctx
    type t = By.t

    val init        : unit -> ctx
    val feed        : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bi.t -> unit
    val get         : ctx -> t

    val digest      : buffer -> t
    val digestv     : buffer list -> t
    val hmac        : key:buffer -> buffer -> t
    val hmacv       : key:buffer -> buffer list -> t

    val compare : t -> t -> int
    val eq      : t -> t -> bool
    val neq     : t -> t -> bool

    val pp      : Format.formatter -> t -> unit
    val of_hex  : buffer -> t
    val to_hex  : t -> buffer
  end
end

module type Desc =
sig
  val digest_size : int
  val block_size  : int
end

module type BUFFER = Baijiu_buffer.S

module type Convenience =
sig
  type t

  val compare : t -> t -> int
  val eq      : t -> t -> bool
  val neq     : t -> t -> bool
end

module type Hash =
  functor (Buffer : BUFFER) -> sig
    type ctx
    type buffer

    val init : unit -> ctx
    val feed : ctx -> buffer -> int -> int -> unit
    val feed_bytes : ctx -> By.t -> int -> int -> unit
    val feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val get  : ctx -> buffer
  end with type buffer = Buffer.buffer

module Core (Hash : Hash) (D : Desc) =
struct
  let digest_size = D.digest_size
  let block_size  = D.block_size

  module Bigstring =
  struct
    include (Bi : Convenience with type t = Bi.t)
    include Hash (struct include Bi type buffer = t end)
    include Pp.Make (Bi) (D)

    let feed ctx buf =
      feed ctx buf 0 (Bi.length buf)
    let feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)
    let feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let digest buf =
      let t = init () in ( feed t buf; get t)

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )
  end

  module Bytes =
  struct
    include (By : Convenience with type t = By.t)
    include Hash (struct include By type buffer = t end)
    include Pp.Make (By) (D)

    let feed ctx buf =
      feed ctx buf 0 (By.length buf)
    let feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)
    let feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let digest buf =
      let t = init () in ( feed t buf; get t)

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )
  end
end

module Make (H : Hash) (D : Desc) =
struct
  module C = Core (H) (D)

  let block_size  = C.block_size
  and digest_size = C.digest_size
  and ctx_size = 0

  module Bytes =
  struct
    module Xor = Xor.Make (struct include By type buffer = t end)

    include C.Bytes

    let opad = By.init C.block_size (fun _ -> '\x5c')
    let ipad = By.init C.block_size (fun _ -> '\x36')

    let rec norm key =
      match Pervasives.compare (By.length key) C.block_size with
      | 1  -> norm (C.Bytes.digest key)
      | -1 -> By.rpad key C.block_size '\000'
      | _  -> key

    let hmacv ~key msg =
      let key = norm key in
      let outer = Xor.xor key opad in
      let inner = Xor.xor key ipad in
      C.Bytes.digestv [ outer; C.Bytes.digestv (inner :: msg) ]

    let hmac ~key msg = hmacv ~key [ msg ]
  end

  module Bigstring =
  struct
    module Xor = Xor.Make (struct include Bi type buffer = t end)
    include C.Bigstring

    let opad = Bi.init C.block_size (fun _ -> '\x5c')
    let ipad = Bi.init C.block_size (fun _ -> '\x36')

    let rec norm key =
      match Pervasives.compare (Bi.length key) C.block_size with
      | 1  -> norm (C.Bigstring.digest key)
      | -1 -> Bi.rpad key C.block_size '\000'
      | _  -> key

    let hmacv ~key msg =
      let key = norm key in
      let outer = Xor.xor key opad in
      let inner = Xor.xor key ipad in
      C.Bigstring.digestv [ outer; C.Bigstring.digestv (inner :: msg) ]

    let hmac ~key msg = hmacv ~key [ msg ]
  end
end

module NI (B : BUFFER) =
struct
  let not_implemented () = raise (Failure "Not implemented")

  type t = B.buffer
  type buffer = B.buffer
  type ctx = unit

  let init = not_implemented
  let feed ctx buf = not_implemented ()
  let get ctx = not_implemented ()
end

module type Hash' =
  functor (Buffer : BUFFER) -> sig
    type ctx
    type buffer

    val init : unit -> ctx
    val init': buffer -> int -> int -> ctx
    val feed : ctx -> buffer -> int -> int -> unit
    val feed_bytes : ctx -> By.t -> int -> int -> unit
    val feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val get  : ctx -> buffer
  end with type buffer = Buffer.buffer

module Make_BLAKE2B (Hash : Hash') (D : Desc) : S =
struct
  let digest_size = D.digest_size

  module Bytes =
  struct
    include (By : Convenience with type t = By.t)
    include Hash (struct include By type buffer = t end)
    include Pp.Make (By) (D)

    let feed ctx buf =
      feed ctx buf 0 (By.length buf)
    let feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)
    let feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let digest buf =
      let t = init () in ( feed t buf; get t)

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )

    let hmacv ~key msg =
      let ctx = init' key 0 (By.length key) in
      List.iter (fun x -> feed ctx x) msg;
      get ctx

    let hmac ~key msg =
      hmacv ~key [ msg ]
  end

  module Bigstring =
  struct
    include (Bi : Convenience with type t = Bi.t)
    include Hash (struct include Bi type buffer = t end)
    include Pp.Make (Bi) (D)

    let feed ctx buf =
      feed ctx buf 0 (Bi.length buf)
    let feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)
    let feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let digest buf =
      let t = init () in ( feed t buf; get t)

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )

    let hmacv ~key msg =
      let ctx = init' key 0 (Bi.length key) in
      List.iter (fun x -> feed ctx x) msg;
      get ctx

    let hmac ~key msg =
      hmacv ~key [ msg ]
  end
end


module DI =
struct
  let digest_size = 0
  let block_size = 0
end

module MD5     : S = Make (Baijiu_md5.Make) (struct let (digest_size, block_size) = (16, 64) end)
module SHA1    : S = Make (Baijiu_sha1.Make)   (struct let (digest_size, block_size) = (20, 64) end)
module SHA224  : S = Make (Baijiu_sha224.Make) (struct let (digest_size, block_size) = (28, 64) end)
module SHA256  : S = Make (Baijiu_sha256.Make) (struct let (digest_size, block_size) = (32, 64) end)
module SHA384  : S = Make (Baijiu_sha384.Make) (struct let (digest_size, block_size) = (48, 128) end)
module SHA512  : S = Make (Baijiu_sha512.Make) (struct let (digest_size, block_size) = (64, 128) end)
module BLAKE2B = Make_BLAKE2B(Baijiu_blake2b.Make) (struct let (digest_size, block_size) = (64, 128) end)
module RMD160  : S = Make (Baijiu_rmd160.Make) (struct let (digest_size, block_size) = (20, 64) end)

type hash =
  [ `MD5
  | `SHA1
  | `SHA224
  | `SHA256
  | `SHA384
  | `SHA512
  | `BLAKE2B
  | `RMD160 ]

let module_of = function
  | `MD5     -> (module MD5     : S)
  | `SHA1    -> (module SHA1    : S)
  | `SHA224  -> (module SHA224  : S)
  | `SHA256  -> (module SHA256  : S)
  | `SHA384  -> (module SHA384  : S)
  | `SHA512  -> (module SHA512  : S)
  | `BLAKE2B -> (module BLAKE2B : S)
  | `RMD160  -> (module RMD160  : S)

module Bytes =
struct
  let digest hash =
    let module H = (val (module_of hash)) in
    H.Bytes.digest

  let digestv hash =
    let module H = (val (module_of hash)) in
    H.Bytes.digestv

  let mac hash =
    let module H = (val (module_of hash)) in
    H.Bytes.hmac

  let macv hash =
    let module H = (val (module_of hash)) in
    H.Bytes.hmacv

  let of_hex hash =
    let module H = (val (module_of hash)) in
    H.Bytes.of_hex

  let to_hex hash =
    let module H = (val (module_of hash)) in
    H.Bytes.to_hex

  let pp hash =
    let module H = (val (module_of hash)) in
    H.Bytes.pp
end

module Bigstring =
struct
  let digest hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.digest

  let digestv hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.digestv

  let mac hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.hmac

  let macv hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.hmacv

  let of_hex hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.of_hex

  let to_hex hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.to_hex

  let pp hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.pp
end

let digest_size hash =
  let module H = (val (module_of hash)) in
  H.digest_size
