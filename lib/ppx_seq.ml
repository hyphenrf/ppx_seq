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
    [%expr let rec inf x s () = Seq.Cons(x, inf (x + s) s) in [%e body]]
  in
  match a with
  | None ->
    mkgen [%expr let x = [%e b] in inf x (succ x - x) ]
  | Some a ->
    mkgen [%expr let x = [%e a] and y = [%e b] in inf x (y - x) ]

let extend_inf = Extension.V2.declare "seq.inf"
  Extension.Context.expression
  Ast_pattern.(pstr @@ alt_option
    (pstr_eval (pexp_tuple (__ ^:: __ ^:: nil)) nil ^:: nil)
    (pstr_eval __ nil ^:: nil)
  )
  (fun ~loc ~path:_ -> einf ~loc)

let efin ~loc a b c =
  let mkgen body = [%expr
    let rec fin x s y () = let n = x + s in
     if compare x y = compare n x then Seq.Nil else Seq.Cons(x, fin n s y)
    in [%e body]
  ]
  in
  match a with
  | None ->
    mkgen [%expr
      let x = [%e b] and y = [%e c] in
      if compare x y = 0 then Seq.return x else
      fin x (if compare x y < 0 then succ x - x else x - succ x) y
    ]
  | Some a ->
    mkgen [%expr
      let x = [%e a] and v = [%e b] and y = [%e c] in
      if compare x y = 0 then Seq.return x else
      if compare x v = 0 then Seq.empty else
      fin x (v - x) y
    ]
let extend_fin = Extension.V2.declare "seq.fin"
  Extension.Context.expression
  Ast_pattern.(pstr @@ alt_option
    (pstr_eval (pexp_tuple (__ ^:: __ ^:: __ ^:: nil)) nil ^:: nil)
    (pstr_eval (pexp_tuple (__ ^:: __ ^:: nil)) nil ^:: nil)
  )
  (fun ~loc ~path:_ -> efin ~loc)

let rules = List.map Context_free.Rule.extension [
  extend_seq;
  extend_inf;
  extend_fin;
]

let _ = Driver.register_transformation
  ~rules
  "ppx_seq"
