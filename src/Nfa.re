type state = int32;

type transitions = StateMap.t(RangeSetListSetMap.t(StateSet.t));

type t = {
  states: StateSet.t,
  alphabet: RangeSetListSetSet.t,
  transitions,
  start: StateSet.t,
  finals: StateSet.t,
};

let singleton = (start: StateSet.t) => {
  states: start,
  alphabet: RangeSetListSetSet.empty,
  transitions: StateMap.empty,
  start,
  finals: StateSet.empty,
};

let set_finals = (finals: StateSet.t, nfa) => {
  states: nfa.states,
  alphabet: nfa.alphabet,
  transitions: nfa.transitions,
  start: nfa.start,
  finals:
    StateSet.(
      elements(finals) |> List.map(s => find(s, nfa.states)) |> of_list
    ),
};

let add_transition: ((state, RangeSetListSet.t, state), t) => t =
  ((src, ranges, dst), nfa) => {
    states: StateSet.(nfa.states |> add(src) |> add(dst)),
    alphabet: RangeSetListSetSet.add(ranges, nfa.alphabet),
    transitions:
      StateMap.add(
        src,
        switch (StateMap.find(src, nfa.transitions)) {
        | exception Not_found =>
          RangeSetListSetMap.singleton(ranges, StateSet.singleton(dst))
        | ranges_map =>
          RangeSetListSetMap.add(
            ranges,
            switch (RangeSetListSetMap.find(ranges, ranges_map)) {
            | exception Not_found => StateSet.singleton(dst)
            | dsts => StateSet.add(dst, dsts)
            },
            ranges_map,
          )
        },
        nfa.transitions,
      ),
    start: nfa.start,
    finals: nfa.finals,
  };

let count_parents: (state, t) => int =
  (state, nfa) => {
    StateMap.fold(
      (_, ranges_map, count) =>
        RangeSetListSetMap.fold(
          (_, dsts, count) =>
            if (StateSet.mem(state, dsts)) {
              count + 1;
            } else {
              count;
            },
          ranges_map,
          count,
        ),
      nfa.transitions,
      0,
    );
  };

let count_children: (state, t) => int =
  (state, nfa) => {
    StateMap.fold(
      (src, ranges_map, count) =>
        if (Int32.compare(src, state) == 0) {
          RangeSetListSetMap.cardinal(ranges_map) + count;
        } else {
          count;
        },
      nfa.transitions,
      0,
    );
  };

let to_dot: t => string =
  nfa =>
    "digraph {\n"
    ++ "rankdir = LR;\n"
    ++ "node [shape = none; width = 0;] \"\";\n"
    ++ String.concat(
         "\n",
         List.map(
           state => {
             let shape =
               if (StateSet.mem(state, nfa.finals)) {
                 "doublecircle";
               } else {
                 "circle";
               };

             "node [shape = "
             ++ shape
             ++ "] "
             ++ Int32.to_string(state)
             ++ ";";
           },
           StateSet.elements(nfa.states),
         ),
       )
    ++ "\n\"\" -> "
    ++ StateSet.to_string(nfa.start)
    ++ ";\n"
    ++ String.concat(
         "\n",
         List.map(
           ((src, ranges_set_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((ranges_set, dsts)) =>
                   Int32.to_string(src)
                   ++ "->"
                   ++ StateSet.to_string(dsts)
                   ++ " [label=\""
                   ++ RangeSetListSet.to_string(ranges_set)
                   ++ "\"];",
                 RangeSetListSetMap.bindings(ranges_set_map),
               ),
             ),
           StateMap.bindings(nfa.transitions),
         ),
       )
    ++ "\n}\n";

let to_matrix: t => array(array(string)) =
  nfa => {
    let states = Array.of_list(StateSet.elements(nfa.states));
    let dimx = Array.length(states);
    let ranges_set_set =
      StateMap.fold(
        (_, ranges_set_map, ranges_set_set) =>
          RangeSetListSetMap.fold(
            (ranges_set, _, ranges_set_set) =>
              RangeSetListSetSet.add(ranges_set, ranges_set_set),
            ranges_set_map,
            ranges_set_set,
          ),
        nfa.transitions,
        RangeSetListSetSet.empty,
      );

    let alphabet =
      Array.of_list(RangeSetListSetSet.elements(ranges_set_set));
    let dimy = Array.length(alphabet);
    let matrix = Array.make_matrix(dimx + 1, dimy + 1, "");
    for (x in 1 to dimx) {
      let src = states[x - 1];
      matrix[x][0] = Int32.to_string(src);
      for (y in 1 to dimy) {
        let ranges_set = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = RangeSetListSet.to_string(ranges_set);
        };
        matrix[x][y] = (
          switch (
            RangeSetListSetMap.find(
              ranges_set,
              StateMap.find(src, nfa.transitions),
            )
          ) {
          | exception Not_found => ""
          | dsts => StateSet.to_string(dsts)
          }
        );
      };
    };
    matrix;
  };

let accept: (t, string) => bool =
  (nfa, input) => {
    let rec step: (StateSet.t, list(RangeSetListSet.t)) => bool =
      cur_states =>
        fun
        | [] => StateSet.(!is_empty(inter(cur_states, nfa.finals)))
        | [cur_ranges, ...rest] =>
          step(
            StateSet.fold(
              src =>
                StateSet.union(
                  try (
                    StateMap.find(src, nfa.transitions)
                    |> RangeSetListSetMap.find(cur_ranges)
                  ) {
                  | Not_found => StateSet.empty
                  },
                ),
              cur_states,
              StateSet.empty,
            ),
            rest,
          );

    step(nfa.start, RangeSetListSet.explode(input));
  };

let test = () => {
  let nfa =
    singleton(StateSet.singleton(Int32.zero))
    |> add_transition((
         Int32.of_int(0),
         RangeSetListSet.of_char('a'),
         Int32.of_int(1),
       ))
    |> add_transition((
         Int32.of_int(0),
         RangeSetListSet.of_char('a'),
         Int32.of_int(2),
       ))
    |> add_transition((
         Int32.of_int(2),
         RangeSetListSet.of_char('b'),
         Int32.of_int(3),
       ))
    |> add_transition((
         Int32.of_int(3),
         RangeSetListSet.of_char('c'),
         Int32.of_int(4),
       ))
    |> add_transition((
         Int32.of_int(4),
         RangeSetListSet.of_char('c'),
         Int32.of_int(4),
       ))
    |> set_finals(StateSet.of_ints([1, 3, 4]));

  assert(accept(nfa, "a"));
  assert(accept(nfa, "abccccc"));
  assert(accept(nfa, "ab"));
};