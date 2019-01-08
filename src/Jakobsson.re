let fold_linear_character_sequences: Sfa.t => Sfa.t =
  input_sfa => {
    let rec fold_linear_character_sequences:
      (Sfa.state, string, Sfa.state) => (Sfa.state, string, Sfa.state) =
      (src, string, dst) =>
        if (Sfa.count_parents(dst, input_sfa) == 1
            && Sfa.count_children(dst, input_sfa) == 1
            && !StateSet.mem(dst, input_sfa.finals)) {
          let (next_string, next_dst) =
            StringMap.choose(StateMap.find(dst, input_sfa.transitions));
          fold_linear_character_sequences(
            src,
            string ++ next_string,
            next_dst,
          );
        } else {
          (src, string, dst);
        };

    let output_sfa =
      Sfa.singleton(input_sfa.start)
      |> StateMap.fold(
           (src, string_map, sfa) =>
             if (Sfa.count_parents(src, input_sfa) == 1
                 && Sfa.count_children(src, input_sfa) == 1
                 && !StateSet.mem(src, input_sfa.finals)) {
               sfa;
             } else {
               StringMap.fold(
                 (string, dst, sfa) =>
                   Sfa.add_transition(
                     fold_linear_character_sequences(src, string, dst),
                     sfa,
                   ),
                 string_map,
                 sfa,
               );
             },
           input_sfa.transitions,
         )
      |> Sfa.set_finals(input_sfa.finals);

    output_sfa;
  };