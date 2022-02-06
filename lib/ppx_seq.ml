open Ppxlib

(* [eseq] function is somewhat similar to [elist] but constructs a sequence.
   It is not provided by the Ast_builder. *)
let rec eseq ~loc = function
  | [] -> [%expr fun () -> Seq.Nil]
  | x :: xs -> [%expr fun () -> Seq.Cons([%e x], [%e eseq ~loc xs])]
[@@tail_mod_cons]

let expand ~loc ~path:_ = eseq ~loc

let extend_seq = Extension.V2.declare "seq"
  Extension.Context.expression
  Ast_pattern.(
    single_expr_payload (esequence __)
  )
  expand

let extend_nil = Extension.V2.declare "seq.empty"
  Extension.Context.expression
  Ast_pattern.(pstr nil)
  (expand [])

let rules = [
  Context_free.Rule.extension extend_seq;
  Context_free.Rule.extension extend_nil;
]

let _ = Driver.register_transformation
  ~rules
  "ppx_seq"
