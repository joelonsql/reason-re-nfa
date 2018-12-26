/** Brzozowski's DFA minimization algorithm: */

/** Build an NFA by reversing a DFA, inverting transition arrows,
   turning finals states into start states, and the start state into
   the final state */

let reverse: Dfa.t => Nfa.t = (dfa) => {
  Nfa.singleton(dfa.finals)
  |> StateMap.fold(
    (src, char_map, nfa) =>
      CharMap.fold(
        (char, dst, nfa) => Nfa.add_transition((dst, CharSet.singleton(char), src), nfa),
        char_map,
        nfa
      ),
    dfa.transitions
  )
  |> Nfa.set_finals(StateSet.singleton(dfa.start))
};

/** Reverse DFA to build an NFA and determinize, then do the same again */

let minimize: Dfa.t => Dfa.t = (dfa) =>
  reverse(dfa)
  -> RabinScott.determinize
  -> reverse
  -> RabinScott.determinize;
