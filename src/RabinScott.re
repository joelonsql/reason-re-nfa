/** Conversion to DFA via the powerset construction */

/** Available transitions from a set of states */

let transitions = (states, nfa) =>
  StateSet.fold(
    (s, m) => {
      let m' = nfa.Nfa.next(s);
      CharMapStateSet.union((_, s, s') => Some(StateSet.union(s, s')), m, m');
    },
    states,
    CharMapStateSet.empty,
  );

let determinize: Nfa.nfa => Dfa.dfa = {
  nfa => {
    let fresh = {
      let r = ref(0l);
      () => {
        r := Int32.succ(r^);
        r^;
      };
    };
    let rec build = (states, (map, ts, finals)) =>
      switch (StateSetMapState.find(states, map)) {
      | state => (state, map, ts, finals)
      | exception Not_found =>
        let state = fresh();
        let finals =
          if (!StateSet.is_empty(StateSet.inter(states, nfa.Nfa.finals))) {
            StateSet.add(state, finals);
          } else {
            finals;
          };
        let map = StateSetMapState.add(states, state, map);
        let (map, ts, finals) =
          CharMapState.fold(
            (c, ss, (map, ts, finals)) => {
              let (dst, map, ts, finals) = build(ss, (map, ts, finals));
              let ts = Dfa.add_transition((state, c, dst), ts);
              (map, ts, finals);
            },
            transitions(states, nfa),
            (map, ts, finals),
          );

        (state, map, ts, finals);
      };

    let (start, _, trans, finals) =
      build(nfa.Nfa.start, (StateSetMapState.empty, StateMapCharMapState.empty, StateSet.empty));

    let next = s =>
      try (StateMapCharMapState.find(s, trans)) {
      | Not_found => CharMapState.empty
      };
    {start, finals, next};
  };
};
