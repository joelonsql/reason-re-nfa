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

/** A simple NFA interpreter. */

let accept = (nfa, inp) => {
  /*** cur_states is the set of all the current states -- i.e. those states at
       which we have arrived by examining the input up to this point.
       Since the automaton is non-deterministic, encountering a character
       in a given state can cause transitions to multiple different
       states */
  let rec step = (cur_states) =>
    fun
    | [] => StateSet.(!is_empty(inter(cur_states, nfa.finals)))
    | [cur_char, ...rest] => step(
      StateSet.fold(
        dst => StateSet.union(
          try (CharMapStateSet.find(cur_char, nfa.next(dst))) {
          | Not_found => StateSet.empty
          }
        ),
        cur_states,
        StateSet.empty
      ),
      rest
    );
  step(nfa.start, inp);
};

