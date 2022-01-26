let tests = ((* BEGIN TESTS *))

  (* A sequence *)
  ; assert begin List.of_seq [%seq 1;2;3] = [1;2;3] end

  (* Just a Nil thunk, equivalent to Seq.empty *)
  ; assert begin List.of_seq [%seq.empty] = [] end