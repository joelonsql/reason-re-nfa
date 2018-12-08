type state = int32;

type transitions = CharMapStateSet.t(StateSet.t);

type nfa = {
  /** the start states */
  start: StateSet.t,
  /** the final (or "accept") states */
  finals: StateSet.t,
  /** the transition function, that maps a state and a character to a
      set of states */
  next: state => transitions
};

let find_states = (sym, nfa, m) =>
  try (CharMapStateSet.find(sym, nfa.next(m))) {
  | Not_found => StateSet.empty
  };

let flat_map = (f, ss) =>
  StateSet.fold(s => StateSet.union(f(s)), ss, StateSet.empty);
let nextss = (curs, sym, nfa) => flat_map(find_states(sym, nfa), curs);

/** A simple NFA interpreter. */

let accept = (nfa, inp) => {
  /*** cur is the set of all the current states -- i.e. those states at
       which we have arrived by examining the input up to this point.
       Since the automaton is non-deterministic, encountering a character
       in a given state can cause transitions to multiple different
       states */
  let rec step = (cur) =>
    fun
    | [] => StateSet.(!is_empty(inter(cur, nfa.finals)))
    | [c, ...cs] => step(nextss(cur, c, nfa), cs);
  step(nfa.start, inp);
};
