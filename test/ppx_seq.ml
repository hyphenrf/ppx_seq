(* [List.of_seq] only available for OCaml >= 4.07 *)
let rec tolist xs = match xs () with
  | Seq.Nil -> []
  | Cons(x,xs) -> x :: tolist xs

let rec take n s () = match s() with
  | Seq.Cons(x, xs) when n > 0 -> Seq.Cons(x, take (n-1) xs)
  | _ -> Seq.Nil

let tests = ((* BEGIN TESTS *))

  (* Basics *)

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

  (* Infinite Ranges *)

  (* [a, succ a..] *)
  ; assert begin tolist @@ take 3 [%seq.inf 1] = [1;2;3] end

  (* [a, a+n, a+2n..] *)
  ; assert begin tolist @@ take 3 [%seq.inf 1, 3] = [1;3;5] end (* n > 0 *)
  ; assert begin tolist @@ take 3 [%seq.inf 1, 1] = [1;1;1] end (* n = 0 *)
  ; assert begin tolist @@ take 3 [%seq.inf 5, 3] = [5;3;1] end (* n < 0 *)

  (* Finite Ranges *)

  ; assert begin tolist [%seq.fin 1, 3] = [1;2;3] end (* a < b *)
  ; assert begin tolist [%seq.fin 1, 1] = [1]     end (* a = b *)
  ; assert begin tolist [%seq.fin 3, 1] = [3;2;1] end (* a > b *)

  ; assert begin tolist [%seq.fin 2, 4, 7] = [2;4;6] end
  ; assert begin tolist [%seq.fin 1, 3, 7] = [1;3;5;7] end
  ; assert begin tolist [%seq.fin 7, 5, 1] = [7;5;3;1] end

  ; assert begin tolist [%seq.fin 2,5,3] = [2] end
  ; assert begin tolist [%seq.fin 5,2,3] = [5] end

  ; assert begin tolist [%seq.fin 2,5,5] = [2;5] end
  ; assert begin tolist [%seq.fin 5,2,2] = [5;2] end
