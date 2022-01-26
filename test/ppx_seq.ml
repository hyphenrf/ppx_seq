let rec tolist xs = match xs () with
  | Seq.Nil -> []
  | Seq.Cons(x, xs) -> x :: tolist xs

let tests = ((* BEGIN TESTS *))

  (* A sequence *)
  ; assert begin tolist [%seq 1;2;3] = [1;2;3] end

  (* Just a Nil thunk, equivalent to Seq.empty *)
  ; assert begin tolist [%seq.empty] = [] end