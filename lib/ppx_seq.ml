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
  (fun ~loc ~path:_ xs -> eseq ~loc @@ match xs with None -> [] | Some xs -> xs)

let einf ~loc ~path:_ a b =
  let mkgen body =
    [%expr let rec inf x s () = Seq.Cons(x, inf (x + s) s) in [%e body]]
  in
  match a with
  | None ->
    let body = mkgen [%expr inf x (succ x - x) ] in
    [%expr let x = [%e b] in [%e body]]
  | Some a ->
    let body = mkgen [%expr inf x (y - x) ] in
    [%expr let x = [%e a] and y = [%e b] in [%e body]]

let extend_inf = Extension.V2.declare "seq.inf"
  Extension.Context.expression
  Ast_pattern.(pstr @@ alt_option
    (pstr_eval (pexp_tuple (__ ^:: __ ^:: nil)) nil ^:: nil)
    (pstr_eval __ nil ^:: nil)
  )
  einf

let efin ~loc ~path:_ a b c =
  let mkgen body = [%expr
    let rec fin x s y () =
      let n = x + s in
      match compare x y, compare n x with
      | 0, 0 | 1, 1 | -1, -1 -> Seq.Nil
      | _ -> Seq.Cons(x, fin n s y)
    in
    [%e body]
  ]
  in
  match a with
  | None ->
    let body = mkgen [%expr
      match compare x y with 0 -> Seq.return x | _ ->
      fin x (match compare x y with -1 -> succ x - x | _ -> x - succ x) y
    ]
    in
    [%expr let x = [%e b] and y = [%e c] in [%e body]]
  | Some a ->
    let body = mkgen [%expr
      (match compare x y with 0 -> Seq.return x | _ ->
      (match compare x v with 0 -> Seq.empty | _ ->
      fin x (v - x) y))
    ]
    in
    [%expr let x = [%e a] and v = [%e b] and y = [%e c] in [%e body]]

let extend_fin = Extension.V2.declare "seq.fin"
  Extension.Context.expression
  Ast_pattern.(pstr @@ alt_option
    (pstr_eval (pexp_tuple (__ ^:: __ ^:: __ ^:: nil)) nil ^:: nil)
    (pstr_eval (pexp_tuple (__ ^:: __ ^:: nil)) nil ^:: nil)
  )
  efin

let rules = List.map Context_free.Rule.extension [
  extend_seq;
  extend_inf;
  extend_fin;
]

let _ = Driver.register_transformation
  ~rules
  "ppx_seq"
