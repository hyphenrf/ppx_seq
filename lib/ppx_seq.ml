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

let rules = [
  Context_free.Rule.extension extend_seq;
]

let _ = Driver.register_transformation
  ~rules
  "ppx_seq"
