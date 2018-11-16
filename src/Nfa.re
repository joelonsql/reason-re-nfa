type state = int32;
module StateSet = {
  module S = Set.Make(Int32);
  let to_string = (ss) =>
    "{" ++ String.concat(" ", List.map(Int32.to_string, S.elements(ss))) ++ "}";
};

module CharMapStateSet = {
  module M = Map.Make(Char);
  let to_string = (cm) =>
    M.fold(
      fun (c,ss,str) => {str ++ " " ++ String.make(1,c) ++ ":" ++ StateSet.to_string(ss)},
      cm,
      ""
    );
};
type transitions = CharMapStateSet.M.t(StateSet.S.t);

type nfa = {
  /** the start state */
  start: state,
  /** the final (or "accept") states */
  finals: StateSet.S.t,
  /** the transition function, that maps a state and a character to a
      set of states */
  next: state => transitions,
  annotated: string,
  nullable: bool,
  firsts: string,
  lasts: string,
  factors: string
};

let find_states = (sym, nfa, m) =>
  try (CharMapStateSet.M.find(sym, nfa.next(m))) {
  | Not_found => StateSet.S.empty
  };

let flat_map = (f, ss) =>
  StateSet.S.fold(s => StateSet.S.union(f(s)), ss, StateSet.S.empty);
let nextss = (curs, sym, nfa) => flat_map(find_states(sym, nfa), curs);

/** A simple NFA interpreter. */

let accept = (nfa, inp) => {
  /*** cur is the set of all the current states -- i.e. those states at
       which we have arrived by examining the input up to this point.
       Since the automaton is non-deterministic, encountering a character
       in a given state can cause transitions to multiple different
       states */
  let rec step = cur =>
    fun
    | [] => StateSet.S.(!is_empty(inter(cur, nfa.finals)))
    | [c, ...cs] => step(nextss(cur, c, nfa), cs);
  step(StateSet.S.singleton(nfa.start), inp);
};
