open Ppxlib

(* [eseq] function is somewhat similar to [elist] but constructs a sequence.
   It is not provided by the Ast_builder. *)
let rec eseq ~loc = function
  | [] -> [%expr fun () -> Seq.Nil]
  | x :: xs -> [%expr fun () -> Seq.Cons([%e x], [%e eseq ~loc xs])]
[@@tail_mod_cons]

let expand ~ctxt exprs =
  eseq exprs
    ~loc:(Expansion_context.Extension.extension_point_loc ctxt)

let extend_seq = Extension.V3.declare "seq"
  Extension.Context.expression
  Ast_pattern.(
    single_expr_payload (esequence __)
  )
  expand

let extend_nil = Extension.V3.declare "seq.empty"
  Extension.Context.expression
  Ast_pattern.__
  (fun ~ctxt _ -> expand ~ctxt [])

let rules = [
  Context_free.Rule.extension extend_seq;
  Context_free.Rule.extension extend_nil;
]

let _ = Driver.register_transformation
  ~rules
  "ppx_seq"
