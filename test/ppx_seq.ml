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

  (* BUG: due to the fact we're using pexp_sequence as a hack to sort of get a list-like syntax,
          [%seq (ignore 1; 1); 2] works as expected, but [%seq 1; (ignore 2; 2)] desugars to the
          same expression as [%seq 1; ignore 2; 2].

          It appears that there's no way to tell, really, from the parsetree, which is which, as
          parens don't make it there by convention.

          to give up on the nice [%seq a; b; c] syntax seems to be the only way to resolve this.
          To my knowledge, we can't even give users meaningful error messages on why right-
          associative parens are the only ones that don't work.

          The current workaround for [%seq 1; (ignore 2; 2)] is [%seq 1; let _ = ignore 2 in 2]
          and similar kinds of solutions that avoid a sequence-in-parens.

          Moving forward, we have to decide whether we want to keep the syntax or fix the problem.
          to fix the problem, we can either say [%seq a, b, c] or [%seq [a; b; c]], both are quite
          unfortunate.
  *)

  (* Infinite Ranges *)

  (* [a, succ a..] *)
  ; assert begin tolist @@ take 3 [%seq.inf 1] = [1;2;3] end

  (* [a, a+n, a+2n..] *)
  ; assert begin tolist @@ take 3 [%seq.inf 1, 3] = [1;3;5] end (* n > 0 *)
  ; assert begin tolist @@ take 3 [%seq.inf 1, 1] = [1;1;1] end (* n = 0 *)
  ; assert begin tolist @@ take 3 [%seq.inf 5, 3] = [5;3;1] end (* n < 0 *)

  (* Finite Ranges *)

  (* NOTE:
     Unlike Haskell, finite ranges are always finite.
     That is, as long as [compare], [+], [-], [succ] are defined in local scope in a sane way.
     With that, any fin expression that could possibly produce an inf sequence is empty seq *)

  (* [a..b] *)
  ; assert begin tolist [%seq.fin 1, 3] = [1;2;3] end (* a < b *)
  ; assert begin tolist [%seq.fin 1, 1] = [1]     end (* a = b *)
  ; assert begin tolist [%seq.fin 3, 1] = [3;2;1] end (* a > b *)

  (* [a, a+n..b] *)
  (* a = b *)
  ; assert begin tolist [%seq.fin 1, 2, 1] = [1] end
  ; assert begin tolist [%seq.fin 1, 1, 1] = [1] end
  ; assert begin tolist [%seq.fin 1, 0, 1] = [1] end
  (* a > b *)
  ; assert begin tolist [%seq.fin 2, 3, 1] = [] end
  ; assert begin tolist [%seq.fin 2, 2, 1] = [] end
  ; assert begin tolist [%seq.fin 2, 1, 1] = [2;1] end
  (* a < b *)
  ; assert begin tolist [%seq.fin 1, 2, 2] = [1;2] end
  ; assert begin tolist [%seq.fin 1, 1, 2] = [] end
  ; assert begin tolist [%seq.fin 1, 0, 2] = [] end

  (* Range Effects *)

  (* NOTE: unlike [[%seq ...]] literals, range evaluation is eager *)
  (* TODO: is there a way to make it lazy? *)

  ; begin
      let module E =
        struct
          let succ () = ()
          let (+) () () = ()
          let (-) () () = ()
          let compare () () = 0
        end
      in
      let r = ref 0 in
      ignore E.(([%seq.inf incr r] : unit Seq.t)); (* TODO: fix typing for warning 5 *)
        assert (!r = 1);
      ignore E.(([%seq.inf incr r, incr r] : unit Seq.t));
        assert (!r = 3);
      ignore E.(([%seq.fin incr r, incr r] : unit Seq.t));
        assert (!r = 5);
      ignore E.(([%seq.fin incr r, incr r, incr r] : unit Seq.t));
        assert (!r = 8);
    end

  (* Misc Finite Ranges *)

  ; assert begin tolist [%seq.fin 2, 4, 7] = [2;4;6] end
  ; assert begin tolist [%seq.fin 1, 3, 7] = [1;3;5;7] end
  ; assert begin tolist [%seq.fin 7, 5, 1] = [7;5;3;1] end

  ; assert begin tolist [%seq.fin 2,5,3] = [2] end
  ; assert begin tolist [%seq.fin 5,2,3] = [5] end

  ; assert begin tolist [%seq.fin 2,5,5] = [2;5] end
  ; assert begin tolist [%seq.fin 5,2,2] = [5;2] end

  (* Hygiene *)

  ; assert begin
      let fin = 1 in tolist [%seq.fin fin, fin+1] = [1;2]
    end
  ; assert begin
      let inf = 1 in tolist @@ take 2 [%seq.inf inf] = [1;2]
    end
