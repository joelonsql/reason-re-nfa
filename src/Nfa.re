type state = int32;

type transitions = StateMap.t(StringMap.t(StateSet.t));

type t = {
  states: StateSet.t,
  alphabet: StringSet.t,
  transitions,
  start: StateSet.t,
  finals: StateSet.t,
};

let singleton = (start: StateSet.t) => {
  states: start,
  alphabet: StringSet.empty,
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

let add_transition: ((state, string, state), t) => t =
  ((src, string, dst), nfa) => {
    states: StateSet.(nfa.states |> add(src) |> add(dst)),
    alphabet: StringSet.add(string, nfa.alphabet),
    transitions:
      StateMap.add(
        src,
        switch (StateMap.find(src, nfa.transitions)) {
        | exception Not_found =>
          StringMap.singleton(string, StateSet.singleton(dst))
        | string_map =>
          StringMap.add(
            string,
            switch (StringMap.find(string, string_map)) {
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
        StringMap.fold(
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
          StringMap.cardinal(string_map) + count;
        } else {
          count;
        },
      nfa.transitions,
      0,
    );
  };

let group_by: transitions => StateMap.t(StringSetMap.t(StateSet.t)) =
  transitions =>
    StateMap.fold(
      (src, string_map, acc) =>
        StateSetMap.fold(
          (dsts, string_set, acc) =>
            StateMap.add(
              src,
              switch (StateMap.find(src, acc)) {
              | exception Not_found =>
                StringSetMap.singleton(string_set, dsts)
              | string_set_map =>
                StringSetMap.add(string_set, dsts, string_set_map)
              },
              acc,
            ),
          StringMap.fold(
            (string, dsts, dsts_map) =>
              StateSetMap.add(
                dsts,
                switch (StateSetMap.find(dsts, dsts_map)) {
                | exception Not_found => StringSet.singleton(string)
                | string_set => StringSet.add(string, string_set)
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
                   ++ StringSet.to_string(string_set)
                   ++ "\"];",
                 StringSetMap.bindings(string_set_map),
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
          StringSetMap.fold(
            (string_set, _, string_set_set) =>
              StringSetSet.add(string_set, string_set_set),
            string_set_map,
            string_set_set,
          ),
        string_set_transitions,
        StringSetSet.empty,
      );

    let alphabet = Array.of_list(StringSetSet.elements(string_set_set));
    let dimy = Array.length(alphabet);
    let matrix = Array.make_matrix(dimx + 1, dimy + 1, "");
    for (x in 1 to dimx) {
      let src = states[x - 1];
      matrix[x][0] = Int32.to_string(src);
      for (y in 1 to dimy) {
        let string_set = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = StringSet.to_string(string_set);
        };
        matrix[x][y] = (
          switch (
            StringSetMap.find(
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

let accept: (t, string) => bool =
  (nfa, input) => {
    let rec step: (StateSet.t, list(string)) => bool =
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
                    |> StringMap.find(cur_string)
                  ) {
                  | Not_found => StateSet.empty
                  },
                ),
              cur_states,
              StateSet.empty,
            ),
            rest,
          );

    step(nfa.start, Common.explode_string(input));
  };

let test = () => {
  let nfa =
    singleton(StateSet.singleton(Int32.zero))
    |> add_transition((Int32.of_int(0), "a", Int32.of_int(1)))
    |> add_transition((Int32.of_int(0), "a", Int32.of_int(2)))
    |> add_transition((Int32.of_int(2), "b", Int32.of_int(3)))
    |> add_transition((Int32.of_int(3), "c", Int32.of_int(4)))
    |> add_transition((Int32.of_int(4), "c", Int32.of_int(4)))
    |> set_finals(StateSet.of_ints([1, 3, 4]));

  assert(accept(nfa, "a"));
  assert(accept(nfa, "abccccc"));
  assert(accept(nfa, "ab"));
};