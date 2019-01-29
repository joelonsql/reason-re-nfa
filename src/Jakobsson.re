/*
 let fold_linear_character_sequences: Nfa.t => Nfa.t =
   input_nfa => {
     let rec fold_linear_character_sequences:
       (Nfa.state, string, Nfa.state) => (Nfa.state, string, Nfa.state) =
       (src, string, dst) =>
         if (Nfa.count_parents(dst, input_nfa) == 1
             && Nfa.count_children(dst, input_nfa) == 1
             && !StateSet.mem(dst, input_nfa.finals)) {
           let (next_string, next_dst) =
             StringMap.choose(StateMap.find(dst, input_nfa.transitions));
           fold_linear_character_sequences(
             src,
             string ++ next_string,
             StateSet.choose_strict(next_dst),
           );
         } else {
           (src, string, dst);
         };

     let output_nfa =
       Nfa.singleton(input_nfa.start)
       |> StateMap.fold(
            (src, ranges_map, nfa) =>
              if (Nfa.count_parents(src, input_nfa) == 1
                  && Nfa.count_children(src, input_nfa) == 1
                  && !StateSet.mem(src, input_nfa.finals)) {
                nfa;
              } else {
                StringMap.fold(
                  (string, dsts, nfa) =>
                    StateSet.fold(
                      (dst, nfa) =>
                        Nfa.add_transition(
                          fold_linear_character_sequences(src, string, dst),
                          nfa,
                        ),
                      dsts,
                      nfa,
                    ),
                  ranges_map,
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
            (src, ranges_map, nfa) => {
              let min_width =
                StringMap.fold(
                  (string, _, min_width) =>
                    if (String.length(string) < min_width || min_width == 0) {
                      String.length(string);
                    } else {
                      min_width;
                    },
                  ranges_map,
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
                ranges_map,
                nfa,
              );
            },
            input_nfa.transitions,
          )
       |> Nfa.set_finals(input_nfa.finals);

     output_nfa;
   };

 let inline: Nfa.t => Nfa.t =
   input_nfa => {
     let rec fold_linear_character_sequences:
       (Nfa.state, string, Nfa.state, list(Nfa.state), string, Nfa.t) => Nfa.t =
       (src, string, dst, srcs, strings, nfa) =>
         if (!List.mem(dst, srcs) && !StateSet.mem(dst, input_nfa.finals)) {
           print_endline(
             "if src "
             ++ Int32.to_string(src)
             ++ " dst "
             ++ Int32.to_string(dst)
             ++ " srcs "
             ++ String.concat(",", List.map(s => Int32.to_string(s), srcs)),
           );
           StringMap.fold(
             (string', dsts, nfa) => {
               let dst' = StateSet.choose_strict(dsts);
               fold_linear_character_sequences(
                 src,
                 strings ++ string ++ string',
                 dst',
                 [dst, ...srcs],
                 "",
                 nfa,
               );
             },
             StateMap.find(dst, input_nfa.transitions), /* find will succeed since we know dst is not a final state */
             nfa,
           );
         } else {
           print_endline(
             "else src "
             ++ Int32.to_string(src)
             ++ " dst "
             ++ Int32.to_string(dst),
           );
           let nfa = Nfa.add_transition((src, strings ++ string, dst), nfa);
           let src = dst;
           StringMap.fold(
             (string, dsts, nfa) =>
               fold_linear_character_sequences(
                 src,
                 string,
                 StateSet.choose_strict(dsts),
                 [src],
                 "",
                 nfa,
               ),
             try (StateMap.find(src, input_nfa.transitions)) {
             | Not_found => StringMap.empty
             },
             nfa,
           );
         };

     let src = StateSet.choose_strict(input_nfa.start);
     StringMap.fold(
       (string, dsts, nfa) =>
         fold_linear_character_sequences(
           src,
           string,
           StateSet.choose_strict(dsts),
           [src],
           "",
           nfa,
         ),
       StateMap.find(src, input_nfa.transitions),
       Nfa.singleton(input_nfa.start),
     );
   };
   */