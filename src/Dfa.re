/** Deterministic finite automata */;

type state = Nfa.state;

type dfa = {
  /** the start state */
  start: state,
  /** the final (or "accept") states */
  finals: StateSet.t,
  /** the transition function, that maps a state and a character to the
      next state */
  next: state => CharMapStateSet.t(state),
};

let fold_states: 'a. ((state, 'a) => 'a, dfa, 'a) => 'a =
  (f, dfa, init) => {
    let v = ref(init);
    let seen = Hashtbl.create(10);
    let rec visit = state =>
      if (!Hashtbl.mem(seen, state)) {
        v := f(state, v^);
        Hashtbl.add(seen, state, ());
        CharMapStateSet.iter(_ => visit, dfa.next(state));
      };
    visit(dfa.start);
    v^;
  };

let fold_transitions: 'a. (((state, char, state), 'a) => 'a, dfa, 'a) => 'a =
  (f, dfa, init) =>
    fold_states(
      (src, v) =>
        CharMapStateSet.fold((c, dst) => f((src, c, dst)), dfa.next(src), v),
      dfa,
      init,
    );

/** Add src--c-->dst to the transition set, replacing any existing src--c-->dst' */

let add_transition = ((src, c, dst), trans) =>
  switch (StateMapCharMapStateSet.find(src, trans)) {
  | exception Not_found =>
    StateMapCharMapStateSet.add(src, CharMapStateSet.singleton(c, dst), trans)
  | cm => StateMapCharMapStateSet.add(src, CharMapStateSet.add(c, dst, cm), trans)
  };

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

/** A simple DFA interpreter. */

let accept = (dfa, inp) => {
  let rec step = cur =>
    fun
    | [] => StateSet.mem(cur, dfa.finals)
    | [c, ...cs] =>
      switch (CharMapStateSet.find(c, dfa.next(cur))) {
      | exception Not_found => false
      | s => step(s, cs)
      };
  step(dfa.start, inp);
};