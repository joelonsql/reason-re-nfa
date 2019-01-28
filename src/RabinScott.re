/** Conversion to DFA via the powerset construction */;

let determinize: Nfa.t => Dfa.t =
  nfa => {
    let rec build: (StateSet.t, Dfa.t, string) => Dfa.t =
      (dfa_src, dfa, indent) =>
        if (StateSetMap.mem(dfa_src, dfa.transitions)) {
          dfa;
        } else {
          dfa
          |> Dfa.set_finals(
               if (!StateSet.is_empty(StateSet.inter(dfa_src, nfa.finals))) {
                 StateSetSet.add(dfa_src, dfa.finals);
               } else {
                 dfa.finals;
               },
             )
          |> CharSetListMap.fold(
               (string, dfa_dst, dfa) => {
                 print_endline(
                   "Adding "
                   ++ StateSet.to_string(dfa_src)
                   ++ " "
                   ++ CharSetList.to_string(string)
                   ++ " "
                   ++ StateSet.to_string(dfa_dst),
                 );
                 let dfa =
                   Dfa.add_transition((dfa_src, string, dfa_dst), dfa);
                 let dfa = build(dfa_dst, dfa, indent ++ "  ");
                 dfa;
               },
               StateSet.fold(
                 (nfa_src, dfa_map) => {
                   let nfa_map =
                     try (StateMap.find(nfa_src, nfa.transitions)) {
                     | Not_found => CharSetListMap.empty
                     };

                   CharSetListMap.union(
                     (_, dst, dst') => Some(StateSet.union(dst, dst')),
                     dfa_map,
                     nfa_map,
                   );
                 },
                 dfa_src,
                 CharSetListMap.empty,
               ),
             );
        };
    let dfa = build(nfa.start, Dfa.singleton(nfa.start), "");
    print_endline("done");
    dfa;
  };

let test = () => {
  let nfa =
    Nfa.singleton(StateSet.singleton(Int32.zero))
    |> Nfa.add_transition((
         Int32.of_int(0),
         CharSetList.of_string("a"),
         Int32.of_int(1),
       ))
    |> Nfa.add_transition((
         Int32.of_int(0),
         CharSetList.of_string("a"),
         Int32.of_int(2),
       ))
    |> Nfa.add_transition((
         Int32.of_int(2),
         CharSetList.of_string("b"),
         Int32.of_int(3),
       ))
    |> Nfa.add_transition((
         Int32.of_int(3),
         CharSetList.of_string("c"),
         Int32.of_int(4),
       ))
    |> Nfa.add_transition((
         Int32.of_int(4),
         CharSetList.of_string("c"),
         Int32.of_int(4),
       ))
    |> Nfa.set_finals(StateSet.of_ints([1, 3, 4]));

  let dfa = determinize(nfa);
  assert(Dfa.accept(dfa, "abccc"));
  assert(!Dfa.accept(dfa, "ab"));
};