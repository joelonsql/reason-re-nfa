type state = int32;
module StateSet: Set.S with type elt = int32;
module CharMap: Map.S with type key = char;
type transitions = CharMap.t(StateSet.t);

type nfa = {
  /** the start state */
  start: state,
  /** the final (or "accept") states */
  finals: StateSet.t,
  /** the transition function, that maps a state and a character to a
      set of states */
  next: state => transitions,
  nullable: bool,
  firsts: string,
  lasts: string,
  pairs: string
};

/** [accept nfa l] is [true] iff the nfa [nfa] accepts the
    character sequence [l] */

let accept: (nfa, list(char)) => bool;
