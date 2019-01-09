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

let align_strings: Sfa.t => Sfa.t =
  input_sfa => {
    let output_sfa =
      Sfa.singleton(input_sfa.start)
      |> StateMap.fold(
           (src, string_map, sfa) => {
             let min_width =
               StringMap.fold(
                 (string, _, min_width) =>
                   if (String.length(string) < min_width || min_width == 0) {
                     String.length(string);
                   } else {
                     min_width;
                   },
                 string_map,
                 0,
               );
             StringMap.fold(
               (string, dst, sfa) => {
                 let string_length = String.length(string);
                 if (string_length == min_width) {
                   Sfa.add_transition((src, string, dst), sfa);
                 } else {
                 let new_state =
                   Int32.succ(StateSet.max_elt(StateSet.union(sfa.Sfa.states,input_sfa.Sfa.states)));
                   sfa
                   |> Sfa.add_transition((
                        src,
                        String.sub(string, 0, min_width),
                        new_state,
                      ))
                   |> Sfa.add_transition((
                        new_state,
                        String.sub(
                          string,
                          min_width,
                          string_length - min_width,
                        ),
                        dst,
                      ));
                 };
               },
               string_map,
               sfa,
             );
           },
           input_sfa.transitions,
         )
      |> Sfa.set_finals(input_sfa.finals);

    output_sfa;
  };