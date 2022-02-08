open Ppxlib

(* [eseq] function is somewhat similar to [elist] but constructs a sequence.
   It is not provided by the Ast_builder. *)
let rec eseq ~loc = function
  | [] -> [%expr fun () -> Seq.Nil]
  | x :: xs -> [%expr fun () -> Seq.Cons([%e x], [%e eseq ~loc xs])]
[@@tail_mod_cons]

let extend_seq = Extension.V2.declare "seq"
  Extension.Context.expression
  Ast_pattern.(pstr @@ alt_option
    (pstr_eval (esequence __) nil ^:: nil)
    nil
  )
  (fun ~loc ~path:_ xs -> eseq ~loc @@ Option.value ~default:[] xs)

let einf ~loc a b =
  let mkgen body =
    [%expr let rec gen x s () = Seq.Cons(x, gen (x + s) s) in [%e body]]
  in
  match a with
  | None ->
    mkgen [%expr let x = [%e b] in gen x (succ x - x) ]
  | Some a ->
    mkgen [%expr let x = [%e a]
                 and y = [%e b] in gen x (y - x) ]

let extend_inf = Extension.V2.declare "seq.inf"
  Extension.Context.expression
  Ast_pattern.(pstr @@ alt_option
    (pstr_eval (pexp_tuple (__ ^:: __ ^:: nil)) nil ^:: nil)
    (pstr_eval __ nil ^:: nil)
  )
  (fun ~loc ~path:_ -> einf ~loc)

let rules = List.map Context_free.Rule.extension [
  extend_seq;
  extend_inf;
]

let _ = Driver.register_transformation
  ~rules
  "ppx_seq"
