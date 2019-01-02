type state = int32;

type transitions = StateMap.t(CharMap.t(StateSet.t));

type t = {
  states: StateSet.t,
  alphabet: CharSet.t,
  transitions,
  start: StateSet.t,
  finals: StateSet.t,
};

let singleton = (start: StateSet.t) => {
  states: start,
  alphabet: CharSet.empty,
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

let add_transition: ((state, char, state), t) => t =
  ((src, char, dst), nfa) => {
    states: StateSet.(nfa.states |> add(src) |> add(dst)),
    alphabet: CharSet.add(char, nfa.alphabet),
    transitions:
      StateMap.add(
        src,
        switch (StateMap.find(src, nfa.transitions)) {
        | exception Not_found =>
          CharMap.singleton(char, StateSet.singleton(dst))
        | char_map =>
          CharMap.add(
            char,
            switch (CharMap.find(char, char_map)) {
            | exception Not_found => StateSet.singleton(dst)
            | dsts => StateSet.add(dst, dsts)
            },
            char_map,
          )
        },
        nfa.transitions,
      ),
    start: nfa.start,
    finals: nfa.finals,
  };

let group_by: transitions => StateMap.t(CharSetMap.t(StateSet.t)) =
  transitions =>
    StateMap.fold(
      (src, charmap, acc) =>
        StateSetMap.fold(
          (dsts, char_set, acc) =>
            StateMap.add(
              src,
              switch (StateMap.find(src, acc)) {
              | exception Not_found => CharSetMap.singleton(char_set, dsts)
              | char_set_map => CharSetMap.add(char_set, dsts, char_set_map)
              },
              acc,
            ),
          CharMap.fold(
            (char, dsts, dsts_map) =>
              StateSetMap.add(
                dsts,
                switch (StateSetMap.find(dsts, dsts_map)) {
                | exception Not_found => CharSet.singleton(char)
                | char_set => CharSet.add(char, char_set)
                },
                dsts_map,
              ),
            charmap,
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
           ((src, char_set_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((char_set, dsts)) =>
                   Int32.to_string(src)
                   ++ "->"
                   ++ StateSet.to_string(dsts)
                   ++ " [label=\""
                   ++ CharSet.to_string(char_set)
                   ++ "\"];",
                 CharSetMap.bindings(char_set_map),
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
    let grouped_transitions = group_by(nfa.transitions);
    let char_set_set =
      StateMap.fold(
        (_, char_set_map, char_set_set) =>
          CharSetMap.fold(
            (char_set, _, char_set_set) =>
              CharSetSet.add(char_set, char_set_set),
            char_set_map,
            char_set_set,
          ),
        grouped_transitions,
        CharSetSet.empty,
      );
    let alphabet = Array.of_list(CharSetSet.elements(char_set_set));
    let dimy = Array.length(alphabet);
    let matrix = Array.make_matrix(dimx + 1, dimy + 1, "");
    for (x in 1 to dimx) {
      let src = states[x - 1];
      matrix[x][0] = Int32.to_string(src);
      for (y in 1 to dimy) {
        let char_set = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = CharSet.to_string(char_set);
        };
        matrix[x][y] = (
          switch (
            CharSetMap.find(
              char_set,
              StateMap.find(src, grouped_transitions),
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
    let rec step: (StateSet.t, list(char)) => bool =
      cur_states =>
        fun
        | [] => StateSet.(!is_empty(inter(cur_states, nfa.finals)))
        | [cur_char, ...rest] =>
          step(
            StateSet.fold(
              src =>
                StateSet.union(
                  try (
                    StateMap.find(src, nfa.transitions)
                    |> CharMap.find(cur_char)
                  ) {
                  | Not_found => StateSet.empty
                  },
                ),
              cur_states,
              StateSet.empty,
            ),
            rest,
          );

    step(nfa.start, Common.explode(input));
  };

let test = () => {
  let nfa =
    singleton(StateSet.singleton(Int32.zero))
    |> add_transition((Int32.of_int(0), 'a', Int32.of_int(1)))
    |> add_transition((Int32.of_int(0), 'a', Int32.of_int(2)))
    |> add_transition((Int32.of_int(2), 'b', Int32.of_int(3)))
    |> add_transition((Int32.of_int(3), 'c', Int32.of_int(4)))
    |> add_transition((Int32.of_int(4), 'c', Int32.of_int(4)))
    |> set_finals(StateSet.of_ints([1, 3, 4]));
  assert(accept(nfa, "a"));
  assert(accept(nfa, "abccccc"));
  assert(accept(nfa, "ab"));
};