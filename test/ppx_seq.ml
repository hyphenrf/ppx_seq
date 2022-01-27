let tests = ((* BEGIN TESTS *))

  (* A sequence *)
  ; assert begin List.of_seq [%seq 1;2;3] = [1;2;3] end

  (* Just a Nil thunk, equivalent to Seq.empty *)
  ; assert begin List.of_seq [%seq.empty] = [] end

  (* Evaluation is properly delayed *)
  ; assert begin
      let r = ref 42 in
      let _ = [%seq r := 0] in
         !r = 42
    end
