let fold_linear_character_sequences: (Nfa.t, int) => Nfa.t =
  (input_nfa, max_word_size) => {
    let rec fold_linear_character_sequences:
      (Nfa.state, string, Nfa.state, Nfa.t) => Nfa.t =
      (src, string, dst, nfa) =>
        if (Nfa.count_parents(dst, input_nfa) == 1
            && Nfa.count_children(dst, input_nfa) == 1
            && !StateSet.mem(dst, input_nfa.finals)) {
          let (next_string, next_dsts) =
            StringMap.choose(StateMap.find(dst, input_nfa.transitions));
          let (src, string, nfa) =
            if (String.length(string) == max_word_size) {
              (dst, "", Nfa.add_transition((src, string, dst), nfa));
            } else {
              (src, string, nfa);
            };
          fold_linear_character_sequences(
            src,
            string ++ next_string,
            StateSet.choose_strict(next_dsts),
            nfa,
          );
        } else {
          Nfa.add_transition((src, string, dst), nfa);
        };

    let output_nfa =
      Nfa.singleton(input_nfa.start)
      |> StateMap.fold(
           (src, string_map, nfa) =>
             if (Nfa.count_parents(src, input_nfa) == 1
                 && Nfa.count_children(src, input_nfa) == 1
                 && !StateSet.mem(src, input_nfa.finals)) {
               nfa;
             } else {
               StringMap.fold(
                 (string, dsts, nfa) =>
                   StateSet.fold(
                     (dst, nfa) =>
                       fold_linear_character_sequences(src, string, dst, nfa),
                     dsts,
                     nfa,
                   ),
                 string_map,
                 nfa,
               );
             },
           input_nfa.transitions,
         )
      |> Nfa.set_finals(input_nfa.finals);

    output_nfa;
  };

let align_strings: Nfa.t => Nfa.t =
  input_nfa => {
    let output_nfa =
      Nfa.singleton(input_nfa.start)
      |> StateMap.fold(
           (src, string_map, nfa) => {
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
               (string, dsts, nfa) =>
                 StateSet.fold(
                   (dst, nfa) => {
                     let string_length = String.length(string);
                     if (string_length == min_width) {
                       Nfa.add_transition((src, string, dst), nfa);
                     } else {
                       let new_state =
                         Int32.succ(
                           StateSet.max_elt(
                             StateSet.union(
                               nfa.Nfa.states,
                               input_nfa.Nfa.states,
                             ),
                           ),
                         );
                       nfa
                       |> Nfa.add_transition((
                            src,
                            String.sub(string, 0, min_width),
                            new_state,
                          ))
                       |> Nfa.add_transition((
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
                   dsts,
                   nfa,
                 ),
               string_map,
               nfa,
             );
           },
           input_nfa.transitions,
         )
      |> Nfa.set_finals(input_nfa.finals);

    output_nfa;
  };