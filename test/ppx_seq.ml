(* [List.of_seq] only available for OCaml >= 4.07 *)
let rec tolist xs = match xs () with
  | Seq.Nil -> []
  | Cons(x,xs) -> x :: tolist xs

let tests = ((* BEGIN TESTS *))

  (* A sequence *)
  ; assert begin tolist [%seq 1;2;3] = [1;2;3] end

  (* Just a Nil thunk, equivalent to Seq.empty *)
  ; assert begin tolist [%seq] = [] end

  (* Evaluation is properly delayed *)
  ; assert begin
      let r = ref 42 in
      let _ = [%seq r := 0] in
         !r = 42
    end
