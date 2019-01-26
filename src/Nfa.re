type state = int32;

type transitions = StateMap.t(CharSetListMap.t(StateSet.t));

type t = {
  states: StateSet.t,
  alphabet: CharSetListSet.t,
  transitions,
  start: StateSet.t,
  finals: StateSet.t,
};

let singleton = (start: StateSet.t) => {
  states: start,
  alphabet: CharSetListSet.empty,
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

let add_transition: ((state, CharSetList.t, state), t) => t =
  ((src, string, dst), nfa) => {
    states: StateSet.(nfa.states |> add(src) |> add(dst)),
    alphabet: CharSetListSet.add(string, nfa.alphabet),
    transitions:
      StateMap.add(
        src,
        switch (StateMap.find(src, nfa.transitions)) {
        | exception Not_found =>
          CharSetListMap.singleton(string, StateSet.singleton(dst))
        | string_map =>
          CharSetListMap.add(
            string,
            switch (CharSetListMap.find(string, string_map)) {
            | exception Not_found => StateSet.singleton(dst)
            | dsts => StateSet.add(dst, dsts)
            },
            string_map,
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
      (_, string_map, count) =>
        CharSetListMap.fold(
          (_, dsts, count) =>
            if (StateSet.mem(state, dsts)) {
              count + 1;
            } else {
              count;
            },
          string_map,
          count,
        ),
      nfa.transitions,
      0,
    );
  };

let count_children: (state, t) => int =
  (state, nfa) => {
    StateMap.fold(
      (src, string_map, count) =>
        if (Int32.compare(src, state) == 0) {
          CharSetListMap.cardinal(string_map) + count;
        } else {
          count;
        },
      nfa.transitions,
      0,
    );
  };

let group_by: transitions => StateMap.t(CharSetListSetMap.t(StateSet.t)) =
  transitions =>
    StateMap.fold(
      (src, string_map, acc) =>
        StateSetMap.fold(
          (dsts, string_set, acc) =>
            StateMap.add(
              src,
              switch (StateMap.find(src, acc)) {
              | exception Not_found =>
                CharSetListSetMap.singleton(string_set, dsts)
              | string_set_map =>
                CharSetListSetMap.add(string_set, dsts, string_set_map)
              },
              acc,
            ),
          CharSetListMap.fold(
            (string, dsts, dsts_map) =>
              StateSetMap.add(
                dsts,
                switch (StateSetMap.find(dsts, dsts_map)) {
                | exception Not_found => CharSetListSet.singleton(string)
                | string_set => CharSetListSet.add(string, string_set)
                },
                dsts_map,
              ),
            string_map,
            StateSetMap.empty,
          ),
          acc,
        ),
      transitions,
      StateMap.empty,
    );

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
           ((src, string_set_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((string_set, dsts)) =>
                   Int32.to_string(src)
                   ++ "->"
                   ++ StateSet.to_string(dsts)
                   ++ " [label=\""
                   ++ CharSetListSet.to_string(string_set)
                   ++ "\"];",
                 CharSetListSetMap.bindings(string_set_map),
               ),
             ),
           StateMap.bindings(group_by(nfa.transitions)),
         ),
       )
    ++ "\n}\n";

let to_matrix: t => array(array(string)) =
  nfa => {
    let states = Array.of_list(StateSet.elements(nfa.states));
    let dimx = Array.length(states);
    let string_set_transitions = group_by(nfa.transitions);
    let string_set_set =
      StateMap.fold(
        (_, string_set_map, string_set_set) =>
          CharSetListSetMap.fold(
            (string_set, _, string_set_set) =>
              CharSetListSetSet.add(string_set, string_set_set),
            string_set_map,
            string_set_set,
          ),
        string_set_transitions,
        CharSetListSetSet.empty,
      );

    let alphabet = Array.of_list(CharSetListSetSet.elements(string_set_set));
    let dimy = Array.length(alphabet);
    let matrix = Array.make_matrix(dimx + 1, dimy + 1, "");
    for (x in 1 to dimx) {
      let src = states[x - 1];
      matrix[x][0] = Int32.to_string(src);
      for (y in 1 to dimy) {
        let string_set = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = CharSetListSet.to_string(string_set);
        };
        matrix[x][y] = (
          switch (
            CharSetListSetMap.find(
              string_set,
              StateMap.find(src, string_set_transitions),
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

let explode_charsetlist = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [[CharSet.singleton(s.[i])], ...l]);
    };
  exp(String.length(s) - 1, []);
};

let accept: (t, string) => bool =
  (nfa, input) => {
    let rec step: (StateSet.t, list(CharSetList.t)) => bool =
      cur_states =>
        fun
        | [] => StateSet.(!is_empty(inter(cur_states, nfa.finals)))
        | [cur_string, ...rest] =>
          step(
            StateSet.fold(
              src =>
                StateSet.union(
                  try (
                    StateMap.find(src, nfa.transitions)
                    |> CharSetListMap.find(cur_string)
                  ) {
                  | Not_found => StateSet.empty
                  },
                ),
              cur_states,
              StateSet.empty,
            ),
            rest,
          );

    step(nfa.start, explode_charsetlist(input));
  };

let test = () => {
  let nfa =
    singleton(StateSet.singleton(Int32.zero))
    |> add_transition((
         Int32.of_int(0),
         CharSetList.of_string("a"),
         Int32.of_int(1),
       ))
    |> add_transition((
         Int32.of_int(0),
         CharSetList.of_string("a"),
         Int32.of_int(2),
       ))
    |> add_transition((
         Int32.of_int(2),
         CharSetList.of_string("b"),
         Int32.of_int(3),
       ))
    |> add_transition((
         Int32.of_int(3),
         CharSetList.of_string("c"),
         Int32.of_int(4),
       ))
    |> add_transition((
         Int32.of_int(4),
         CharSetList.of_string("c"),
         Int32.of_int(4),
       ))
    |> set_finals(StateSet.of_ints([1, 3, 4]));

  assert(accept(nfa, "a"));
  assert(accept(nfa, "abccccc"));
  assert(accept(nfa, "ab"));
};