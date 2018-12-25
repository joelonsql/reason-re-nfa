/** Brzozowski's DFA minimization algorithm: */

/** Add src--c-->dst to the transition set, augmenting any existing src--c-->dst' */

let add_transition' = ((src, c, dst), trans) =>
  switch (StateMapCharMapStateSet.find(src, trans)) {
  | exception Not_found =>
    StateMapCharMapStateSet.add(src, CharMapStateSet.singleton(c, StateSet.singleton(dst)), trans)
  | cm =>
    let dstset =
      switch (CharMapStateSet.find(c, cm)) {
      | exception Not_found => StateSet.singleton(dst)
      | dstset => StateSet.add(dst, dstset)
      };
    StateMapCharMapStateSet.add(src, CharMapStateSet.add(c, dstset, cm), trans);
  };

/** Build an NFA by reversing a DFA, inverting transition arrows,
   turning finals states into start states, and the start state into
   the final state */

let reverse = dfa => {
  let map =
    Dfa.fold_transitions(
      ((s, c, t)) => add_transition'((t, c, s)),
      dfa,
      StateMapCharMapStateSet.empty,
    );

  {
    Nfa.start: dfa.finals,
    Nfa.finals: StateSet.singleton(dfa.start),
    next: s =>
      try (StateMapCharMapStateSet.find(s, map)) {
      | Not_found => CharMapStateSet.empty
      },
  };
};

/** Reverse DFA to build an NFA and determinize, then do the same again */

let minimize = dfa => reverse(dfa) -> RabinScott.determinize -> reverse -> RabinScott.determinize;

let inject = ({Dfa.start, Dfa.finals, Dfa.next}) => {
  Nfa.start: StateSet.singleton(start),
  Nfa.finals,
  Nfa.next: s => CharMapStateSet.map(StateSet.singleton, next(s)),
};
