/** Conversion to DFA via the powerset construction */

let flatten_transitions: CharSetMap.t(StateSet.t) => CharMap.t(StateSet.t) = (char_map) =>
  CharSetMap.fold(
    (char_set, state_set, char_map) =>
    CharSet.fold(
      (char, char_map) => {
        let entry =
          switch (CharMap.find(char, char_map)) {
          | exception Not_found => StateSet.empty
          | state_set => state_set
          };
        CharMap.add(char, StateSet.union(state_set, entry), char_map);
      },
      char_set,
      char_map
    ),
    char_map,
    CharMap.empty,
  );

let determinize: Nfa.t => Dfa.t = {
  nfa => {
    let fresh = {
      let r = ref(-1l);
      () => {
        r := Int32.succ(r^);
        r^;
      };
    };
    let rec build = (states, (map, dfa, finals)) =>
      switch (StateSetMap.find(states, map)) {
      | state => (state, map, dfa, finals)
      | exception Not_found =>
        let state = fresh();
        let finals =
          if (!StateSet.is_empty(StateSet.inter(states, nfa.Nfa.finals))) {
            StateSet.add(state, finals);
          } else {
            finals;
          };
        let map = StateSetMap.add(states, state, map);
        let (state, map, dfa, finals) =
          CharMap.fold(
            (c, ss, (state, map, dfa, finals)) => {
              let (dst, map, dfa, finals) = build(ss, (map, dfa, finals));
              let dfa = Dfa.add_transition((state, c, dst), dfa);
              (state, map, dfa, finals);
            },
            StateSet.fold(
              (s, m) => {
                let m' = try (flatten_transitions(StateMap.find(s, nfa.Nfa.transitions))) {
                | Not_found => CharMap.empty
                };
                CharMap.union((_, s, s') => Some(StateSet.union(s, s')), m, m');
              },
              states,
              CharMap.empty,
            ),
            (state, map, dfa, finals),
          );

        (state, map, dfa, finals);
      };

    let (_, _, dfa, finals) =
      build(nfa.Nfa.start, (StateSetMap.empty, Dfa.singleton(Int32.zero), StateSet.empty));
    Dfa.set_finals(finals, dfa);
  };
};

let test = () => {
  let nfa =
    Nfa.singleton(StateSet.singleton(Int32.zero))
    |> Nfa.add_transition((
         Int32.of_int(0),
         CharSet.singleton('a'),
         Int32.of_int(1),
       ))
    |> Nfa.add_transition((
         Int32.of_int(0),
         CharSet.singleton('a'),
         Int32.of_int(2),
       ))
    |> Nfa.add_transition((
         Int32.of_int(2),
         CharSet.singleton('b'),
         Int32.of_int(3),
       ))
    |> Nfa.add_transition((
         Int32.of_int(3),
         CharSet.singleton('c'),
         Int32.of_int(4),
       ))
    |> Nfa.add_transition((
         Int32.of_int(4),
         CharSet.singleton('c'),
         Int32.of_int(4),
       ))
    |> Nfa.set_finals(StateSet.example([1,3,4]));
  Js.log(Nfa.to_matrix(nfa));
  let dfa = determinize(nfa);
  Js.log(Dfa.to_matrix(dfa));
};
