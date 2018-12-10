/** Deterministic finite automata */;

type state = Nfa.state;
module StateMap = Map.Make(Int32);

/* val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t */
let charmap_union = (type a, f: (char, a, a) => option(a)) => {
  let f = (k, x, y) =>
    switch (x, y) {
    | (None, None) => None
    | (Some(v), None) => Some(v)
    | (None, Some(v)) => Some(v)
    | (Some(v1), Some(v2)) => f(k, v1, v2)
    };
  CharMapStateSet.merge(f);
};

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
  switch (StateMap.find(src, trans)) {
  | exception Not_found =>
    StateMap.add(src, CharMapStateSet.singleton(c, dst), trans)
  | cm => StateMap.add(src, CharMapStateSet.add(c, dst, cm), trans)
  };

/** Add src--c-->dst to the transition set, augmenting any existing src--c-->dst' */

let add_transition' = ((src, c, dst), trans) =>
  switch (StateMap.find(src, trans)) {
  | exception Not_found =>
    StateMap.add(src, CharMapStateSet.singleton(c, StateSet.singleton(dst)), trans)
  | cm =>
    let dstset =
      switch (CharMapStateSet.find(c, cm)) {
      | exception Not_found => StateSet.singleton(dst)
      | dstset => StateSet.add(dst, dstset)
      };
    StateMap.add(src, CharMapStateSet.add(c, dstset, cm), trans);
  };

/** Build an NFA by reversing a DFA, inverting transition arrows,
   turning finals states into start states, and the start state into
   the final state */

let reverse = dfa => {
  let map =
    fold_transitions(
      ((s, c, t)) => add_transition'((t, c, s)),
      dfa,
      StateMap.empty,
    );

  {
    Nfa.start: dfa.finals,
    Nfa.finals: StateSet.singleton(dfa.start),
    next: s =>
      try (StateMap.find(s, map)) {
      | Not_found => CharMapStateSet.empty
      },
  };
};

/** Available transitions from a set of states */

let transitions = (states, nfa) =>
  StateSet.fold(
    (s, m) => {
      let m' = nfa.Nfa.next(s);
      charmap_union((_, s, s') => Some(StateSet.union(s, s')), m, m');
    },
    states,
    CharMapStateSet.empty,
  );

/** Conversion to DFA via the powerset construction */

let determinize: Nfa.nfa => dfa = {
  module M = Map.Make(StateSet);
  nfa => {
    let fresh = {
      let r = ref(0l);
      () => {
        r := Int32.succ(r^);
        r^;
      };
    };
    let rec build = (states, (map, ts, finals)) =>
      switch (M.find(states, map)) {
      | state => (state, map, ts, finals)
      | exception Not_found =>
        let state = fresh();
        let finals =
          if (!StateSet.is_empty(StateSet.inter(states, nfa.Nfa.finals))) {
            StateSet.add(state, finals);
          } else {
            finals;
          };
        let map = M.add(states, state, map);
        let tsn = transitions(states, nfa);
        let (map, ts, finals) =
          CharMapStateSet.fold(
            (c, ss, (map, ts, finals)) => {
              let (dst, map, ts, finals) = build(ss, (map, ts, finals));
              let ts = add_transition((state, c, dst), ts);
              (map, ts, finals);
            },
            tsn,
            (map, ts, finals),
          );

        (state, map, ts, finals);
      };

    let (start, _, trans, finals) =
      build(nfa.Nfa.start, (M.empty, StateMap.empty, StateSet.empty));

    let next = s =>
      try (StateMap.find(s, trans)) {
      | Not_found => CharMapStateSet.empty
      };
    {start, finals, next};
  };
};

/** Brzozowski's DFA minimization algorithm:
    reverse DFA to build an NFA and determinize, then do the same again */

let minimize = g => determinize(reverse(determinize(reverse(g))));

let inject = ({start, finals, next}) => {
  Nfa.start: StateSet.singleton(start),
  finals,
  next: s => CharMapStateSet.map(StateSet.singleton, next(s)),
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