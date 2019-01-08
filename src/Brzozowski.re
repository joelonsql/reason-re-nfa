/** Brzozowski's DFA minimization algorithm: */;

/** Build an NFA by assining unique int32 for each DFA StateSet */;

let dfa_to_nfa: Dfa.t => Nfa.t =
  dfa => {
    let (dfa_to_nfa_map, _) =
      StateSetSet.fold(
        (dfa_state, (dfa_to_nfa_map, nfa_state)) =>
          (
            StateSetMap.add(dfa_state, nfa_state, dfa_to_nfa_map),
            Int32.succ(nfa_state),
          ),
        dfa.states,
        (StateSetMap.empty, Int32.zero),
      );

    Nfa.singleton(
      StateSet.singleton(StateSetMap.find(dfa.start, dfa_to_nfa_map)),
    )
    |> StateSetMap.fold(
         (dfa_src, char_map, nfa) =>
           CharMap.fold(
             (char, dfa_dst, nfa) =>
               Nfa.add_transition(
                 (
                   StateSetMap.find(dfa_src, dfa_to_nfa_map),
                   char,
                   StateSetMap.find(dfa_dst, dfa_to_nfa_map),
                 ),
                 nfa,
               ),
             char_map,
             nfa,
           ),
         dfa.transitions,
       )
    |> Nfa.set_finals(
         StateSetSet.fold(
           (dfa_state, nfa_states) =>
             StateSet.add(
               StateSetMap.find(dfa_state, dfa_to_nfa_map),
               nfa_states,
             ),
           dfa.finals,
           StateSet.empty,
         ),
       );
  };

let dfa_to_sfa: Dfa.t => Sfa.t =
  dfa => {
    let (dfa_to_sfa_map, _) =
      StateSetSet.fold(
        (dfa_state, (dfa_to_nfa_map, sfa_state)) =>
          (
            StateSetMap.add(dfa_state, sfa_state, dfa_to_nfa_map),
            Int32.succ(sfa_state),
          ),
        dfa.states,
        (StateSetMap.empty, Int32.zero),
      );

    Sfa.singleton(StateSetMap.find(dfa.start, dfa_to_sfa_map))
    |> StateSetMap.fold(
         (dfa_src, char_map, sfa) =>
           CharMap.fold(
             (char, dfa_dst, sfa) =>
               Sfa.add_transition(
                 (
                   StateSetMap.find(dfa_src, dfa_to_sfa_map),
                   String.make(1, char),
                   StateSetMap.find(dfa_dst, dfa_to_sfa_map),
                 ),
                 sfa,
               ),
             char_map,
             sfa,
           ),
         dfa.transitions,
       )
    |> Sfa.set_finals(
         StateSetSet.fold(
           (dfa_state, sfa_states) =>
             StateSet.add(
               StateSetMap.find(dfa_state, dfa_to_sfa_map),
               sfa_states,
             ),
           dfa.finals,
           StateSet.empty,
         ),
       );
  };

/** Build a reversed NFA by inverting transition arrows,
    turning finals states into start states, and the start state into
    the final state */

let reverse: Nfa.t => Nfa.t =
  nfa =>
    Nfa.singleton(nfa.finals)
    |> StateMap.fold(
         (src, char_map, nfa) =>
           CharMap.fold(
             (char, dsts, nfa) =>
               StateSet.fold(
                 (dst, nfa) => Nfa.add_transition((dst, char, src), nfa),
                 dsts,
                 nfa,
               ),
             char_map,
             nfa,
           ),
         nfa.transitions,
       )
    |> Nfa.set_finals(nfa.start);

/** Convert DFA to NFA, then reverse NFA and determinize to DFA, then do
     the same again */

let minimize: Dfa.t => Dfa.t =
  dfa =>
    dfa_to_nfa(dfa)
    |> reverse
    |> RabinScott.determinize
    |> dfa_to_nfa
    |> reverse
    |> RabinScott.determinize;