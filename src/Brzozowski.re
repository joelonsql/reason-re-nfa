/** Brzozowski's DFA minimization algorithm: */;

/** Build an NFA by assining unique int32 for each DFA StateSet */;

let dfa_to_nfa: Dfa.t => Nfa.t =
  dfa =>
  ;

/** Build an reversed NFA by inverting transition arrows,
    turning finals states into start states, and the start state into
    the final state */

let reverse: Nfa.t => Nfa.t =
  nfa =>
    Nfa.singleton(nfa.finals)
    |> StateMap.fold(
         (src, char_map, nfa) =>
           CharMap.fold(
             (char, dst, nfa) => Nfa.add_transition((dst, char, src), nfa),
             char_map,
             nfa,
           ),
         nfa.transitions,
       )
    |> Nfa.set_finals(StateSet.singleton(nfa.start));

/** Reverse DFA to build an NFA and determinize, then do the same again */

let minimize: Dfa.t => Dfa.t =
  dfa =>
    reverse(dfa)
    |> RabinScott.determinize
    |> reverse
    |> RabinScott.determinize;