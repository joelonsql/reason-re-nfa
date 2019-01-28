/** Conversion to DFA via the powerset construction */;

let determinize: Nfa.t => Dfa.t =
  nfa => {
    let rec build: (StateSet.t, Dfa.t) => Dfa.t =
      (dfa_src, dfa) =>
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
          |> RangeSetMap.fold(
               (ranges, dfa_dst, dfa) =>
                 Dfa.add_transition((dfa_src, ranges, dfa_dst), dfa)
                 |> build(dfa_dst),
               StateSet.fold(
                 (nfa_src, dfa_map) => {
                   let nfa_map =
                     try (StateMap.find(nfa_src, nfa.transitions)) {
                     | Not_found => RangeSetMap.empty
                     };

                   RangeSetMap.union(
                     (_, dst, dst') => Some(StateSet.union(dst, dst')),
                     dfa_map,
                     nfa_map,
                   );
                 },
                 dfa_src,
                 RangeSetMap.empty,
               ),
             );
        };
    build(nfa.start, Dfa.singleton(nfa.start));
  };

let test = () => {
  let nfa =
    Nfa.singleton(StateSet.singleton(Int32.zero))
    |> Nfa.add_transition((
         Int32.of_int(0),
         RangeSet.of_char('a'),
         Int32.of_int(1),
       ))
    |> Nfa.add_transition((
         Int32.of_int(0),
         RangeSet.of_char('a'),
         Int32.of_int(2),
       ))
    |> Nfa.add_transition((
         Int32.of_int(2),
         RangeSet.of_char('b'),
         Int32.of_int(3),
       ))
    |> Nfa.add_transition((
         Int32.of_int(3),
         RangeSet.of_char('c'),
         Int32.of_int(4),
       ))
    |> Nfa.add_transition((
         Int32.of_int(4),
         RangeSet.of_char('c'),
         Int32.of_int(4),
       ))
    |> Nfa.set_finals(StateSet.of_ints([1, 3, 4]));

  let dfa = determinize(nfa);
  assert(Dfa.accept(dfa, "abccc"));
  assert(!Dfa.accept(dfa, "ab"));
};