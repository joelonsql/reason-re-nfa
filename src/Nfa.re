open Common

type state = int32;
type transitions = StateMap.t(CharSetMap.t(StateSet.t));
type t = {
  states: StateSet.t,
  alphabet: CharSetSet.t,
  transitions,
  start: StateSet.t,
  finals: StateSet.t,
};

let singleton = (start: StateSet.t) => {
  states: start,
  alphabet: CharSetSet.empty,
  transitions: StateMap.empty,
  start: start,
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

let add_transition: ((state, CharSet.t, state), t) => t =
  ((src, char_set, dst), nfa) => {
    states: StateSet.(nfa.states |> add(src) |> add(dst)),
    alphabet: CharSetSet.add(char_set, nfa.alphabet),
    transitions:
      StateMap.add(
        src,
        switch (StateMap.find(src, nfa.transitions)) {
        | exception Not_found =>
          CharSetMap.singleton(char_set, StateSet.singleton(dst))
        | char_set_map =>
          CharSetMap.add(
            char_set,
            switch (CharSetMap.find(char_set, char_set_map)) {
            | exception Not_found => StateSet.singleton(dst)
            | dsts => StateSet.add(dst, dsts)
            },
            char_set_map,
          )
        },
        nfa.transitions,
      ),
    start: nfa.start,
    finals: nfa.finals,
  };

let to_dot: t => string =
  nfa =>
    "digraph {\n" ++
    "rankdir = LR;\n" ++
    "node [shape = none; width = 0;] \"\";\n" ++
    String.concat(
      "\n",
      List.map(
        (state) => {
          let shape = if (StateSet.mem(state, nfa.finals)) {
            "doublecircle"
          } else {
            "circle"
          };
          "node [shape = " ++ shape ++ "] " ++ Int32.to_string(state) ++ ";";
        },
        StateSet.elements(nfa.states),
      ),
    ) ++
    "\n\"\" -> " ++ StateSet.to_string(nfa.start) ++ ";\n" ++
    String.concat(
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
        StateMap.bindings(nfa.transitions),
      ),
    ) ++
    "\n}\n";

let to_matrix: t => array(array(string)) =
  nfa => {
    let states = Array.of_list(StateSet.elements(nfa.states));
    let dimx = Array.length(states);
    let alphabet = Array.of_list(CharSetSet.elements(nfa.alphabet));
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
            CharSetMap.find(char_set, StateMap.find(src, nfa.transitions))
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
                  switch (StateMap.find(src, nfa.transitions)) {
                  | exception Not_found => StateSet.empty
                  | char_set_map =>
                    CharSetMap.fold(
                      (_, state_set, dsts) =>
                        StateSet.union(dsts, state_set),
                      CharSetMap.filter(
                        (char_set, _) => CharSet.mem(cur_char, char_set),
                        char_set_map,
                      ),
                      StateSet.empty,
                    )
                  },
                ),
              cur_states,
              StateSet.empty,
            ),
            rest,
          );
    step(nfa.start, explode(input));
  };

let dump = (nfa) => {
  Js.log("Nfa.states: " ++ StateSet.to_string(nfa.states));
  Js.log("Nfa.alphabet: " ++ CharSetSet.to_string(nfa.alphabet));
  Js.log("Nfa.transitions:");
  Js.log(to_matrix(nfa));
  Js.log("Nfa.start: " ++ StateSet.to_string(nfa.start));
  Js.log("Nfa.finals: " ++ StateSet.to_string(nfa.finals));
};

let test = () => {
  let nfa =
    singleton(StateSet.singleton(Int32.zero))
    |> add_transition((
         Int32.of_int(0),
         CharSet.singleton('a'),
         Int32.of_int(1),
       ))
    |> add_transition((
         Int32.of_int(0),
         CharSet.singleton('a'),
         Int32.of_int(2),
       ))
    |> add_transition((
         Int32.of_int(2),
         CharSet.singleton('b'),
         Int32.of_int(3),
       ))
    |> add_transition((
         Int32.of_int(3),
         CharSet.singleton('c'),
         Int32.of_int(4),
       ))
    |> add_transition((
         Int32.of_int(4),
         CharSet.singleton('c'),
         Int32.of_int(4),
       ))
    |> set_finals(StateSet.example([1,3,4]));
  print_endline(to_dot(nfa));
  Js.log(to_matrix(nfa));
  Js.log(accept(nfa, "abc"));
};
