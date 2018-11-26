open Bechamel
open Toolkit

let () = Printexc.record_backtrace true
let block = 50

module Monotonic_clock = struct
  type witness = int
  type value = int64 ref
  type label = string

  let make () = Oclock.monotonic
  let load _witness = ()
  let unload _witness = ()
  let float v = Int64.to_float !v
  let label _witness = "monotonic-clock"
  let diff a b = {contents= Int64.sub !b !a}
  let epsilon () = {contents= 0L}
  let blit witness v = v := Oclock.gettime witness
end

module Block = struct
  type witness = unit
  type value = int
  type label = string

  let make () = ()
  let load _witness = ()
  let unload _witness = ()
  let float v = float_of_int v
  let label _witness = "block"
  let diff a _b = a
  let epsilon () = 0
  let blit _witness _v = ()
end

module Extension = struct
  include Extension

  let monotonic_clock = Measure.make (module Monotonic_clock)
  let block = Measure.make (module Block)
end

module Instance = struct
  include Instance

  let monotonic_clock =
    Measure.instance (module Monotonic_clock) Extension.monotonic_clock

  let block = Measure.instance (module Block) Extension.block
end

(** TESTS **)

let () = Random.self_init ()
let ( <.> ) f g x = f (g x)

let random_bytes len =
  let buf = Bytes.create len in
  let ic = open_in "/dev/urandom" in
  really_input ic buf 0 len ; close_in ic ; buf

let digest_bytes digest len =
  let input = random_bytes len in
  Staged.stage (fun () -> Digestif.digest_bytes digest input)

let block_of_kind : [< Digestif.kind] -> int = function
  | `SHA1 -> 64
  | `BLAKE2B -> 128
  | `BLAKE2S -> 64
  | `SHA256 -> 128
  | `SHA224 -> 128
  | `SHA384 -> 128
  | `SHA512 -> 128
  | `MD5 -> 64
  | `WHIRLPOOL -> 64
  | `RMD160 -> 64

let kind_of_hash : type k. k Digestif.hash -> k =
 fun hash ->
  let module X = (val Digestif.module_of hash) in
  X.kind

let len_list block = List.init block (fun i -> (i + 1) * block)

let test_md5 =
  let hash = Digestif.md5 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.md5" ~args:(len_list block)
    (digest_bytes hash)

let test_sha1 =
  let hash = Digestif.sha1 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.sha1" ~args:(len_list block)
    (digest_bytes hash)

let test_rmd160 =
  let hash = Digestif.rmd160 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.rmd160" ~args:(len_list block)
    (digest_bytes hash)

let test_sha224 =
  let hash = Digestif.sha224 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.sha224" ~args:(len_list block)
    (digest_bytes hash)

let test_sha256 =
  let hash = Digestif.sha256 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.sha256" ~args:(len_list block)
    (digest_bytes hash)

let test_sha384 =
  let hash = Digestif.sha384 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.sha384" ~args:(len_list block)
    (digest_bytes hash)

let test_sha512 =
  let hash = Digestif.sha512 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.sha512" ~args:(len_list block)
    (digest_bytes hash)

let test_whirlpool =
  let hash = Digestif.whirlpool in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.whirlpool" ~args:(len_list block)
    (digest_bytes hash)

let test_blake2b =
  let hash = Digestif.blake2b 64 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.blake2b" ~args:(len_list block)
    (digest_bytes hash)

let test_blake2s =
  let hash = Digestif.blake2s 32 in
  let block = block_of_kind (kind_of_hash hash) in
  Test.make_indexed ~name:"Digestif.blake2s" ~args:(len_list block)
    (digest_bytes hash)

(** TESTS **)

let zip l1 l2 =
  let rec go acc = function
    | [], [] -> List.rev acc
    | x1 :: r1, x2 :: r2 -> go ((x1, x2) :: acc) (r1, r2)
    | _, _ -> assert false
  in
  go [] (l1, l2)

let pp_result ppf result =
  let style_by_r_square =
    match Analyze.OLS.r_square result with
    | Some r_square ->
        if r_square >= 0.95 then `Green
        else if r_square >= 0.90 then `Yellow
        else `Red
    | None -> `None
  in
  match Analyze.OLS.estimates result with
  | Some estimates ->
      Fmt.pf ppf "%a per %a = %a [%a%%]" Label.pp
        (Analyze.OLS.responder result)
        Fmt.(Dump.list Label.pp)
        (Analyze.OLS.predictors result)
        Fmt.(styled style_by_r_square (Dump.list float))
        estimates
        Fmt.(option float)
        (Analyze.OLS.r_square result)
  | None ->
      Fmt.pf ppf "%a per %a = #unable-to-compute" Label.pp
        (Analyze.OLS.responder result)
        Fmt.(Dump.list Label.pp)
        (Analyze.OLS.predictors result)

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let pp ppf (test, results) =
  let tests = Test.set test in
  List.iter
    (fun results ->
      List.iter
        (fun (test, result) ->
          Fmt.pf ppf "@[<hov>%s = %a@]@\n"
            (pad 30 @@ Test.Elt.name test)
            pp_result result )
        (zip tests results) )
    results

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_src_and_stamp h _ k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("%s %a %a: @[" ^^ fmt ^^ "@]@.")
        (pad 20 (Fmt.strf "%+04.0fus" dt))
        Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (pad 20 @@ Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_src_and_stamp header tags k fmt
  in
  {Logs.report}

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stdout) ;
  let quiet = match style_renderer with Some _ -> true | None -> false in
  quiet, Fmt.stdout

let _, _ = setup_logs (Some `Ansi_tty) (Some Logs.Debug)

let pp_dot ~x ~y ppf m =
  let x, pp_x = x in
  let y, pp_y = y in
  Fmt.array
    ~sep:Fmt.(const string "\n")
    Fmt.(pair ~sep:Fmt.(const string ",") pp_x pp_y)
    ppf
    (Array.map
       (fun m -> Measurement_raw.get ~label:x m, Measurement_raw.get ~label:y m)
       m)

let all ~sampling ~stabilize ~quota ~run instances test =
  let tests = Test.set test in
  let module ExtBlock = (val Extension.block) in
  let blocks = List.init (List.length tests) (fun i -> ExtBlock.T ((), i)) in
  List.map
    (fun (test, block) ->
      Fmt.pr "Start to benchmark: %s.\n%!" (Test.Elt.name test) ;
      Benchmark.run ~sampling ~stabilize ~quota run (block :: instances) test
      )
    (zip tests blocks)
  |> Array.concat

let () =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0
      ~predictors:[|Measure.label Instance.block|]
  in
  let instances = Instance.[monotonic_clock] in
  let tests =
    match Sys.argv with
    | [|_|] -> []
    | [|_; "md5"|] -> [test_md5]
    | [|_; "sha1"|] -> [test_sha1]
    | [|_; "rmd160"|] -> [test_rmd160]
    | [|_; "sha224"|] -> [test_sha224]
    | [|_; "sha256"|] -> [test_sha256]
    | [|_; "sha384"|] -> [test_sha384]
    | [|_; "sha512"|] -> [test_sha512]
    | [|_; "whirlpool"|] -> [test_whirlpool]
    | [|_; "blake2b"|] -> [test_blake2b]
    | [|_; "blake2s"|] -> [test_blake2s]
    | [|_; "all"|] ->
        [ test_md5; test_sha1; test_rmd160; test_sha224; test_sha256
        ; test_sha384; test_sha512; test_whirlpool; test_blake2b; test_blake2s
        ]
    | _ ->
        Fmt.invalid_arg
          "%s \
           {md5|sha1|rmd160|sha224|sha256|sha384|sha512|whirlpool|blake2b|blake2s|all}"
          Sys.argv.(1)
  in
  let measure_and_analyze test =
    let result =
      all ~sampling:(`Linear 0) ~stabilize:true ~quota:(Benchmark.s 1.)
        ~run:3000 instances test
    in
    List.map (fun x -> Analyze.analyze ols (Measure.label x) result) instances
  in
  let results = List.map measure_and_analyze tests in
  Fmt.pr "%a\n%!" Fmt.(Dump.list (Dump.list pp_result)) results
