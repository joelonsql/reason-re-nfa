exception Non_deterministic(string);

type state = int32;

type transitions = StateMap.t(StringMap.t(state));

type t = {
  states: StateSet.t,
  alphabet: StringSet.t,
  transitions,
  start: state,
  finals: StateSet.t,
};

let singleton = (start: state) => {
  states: StateSet.singleton(start),
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
  ((src, string, dst), sfa) => {
    states: StateSet.(sfa.states |> add(src) |> add(dst)),
    alphabet: StringSet.add(string, sfa.alphabet),
    transitions:
      StateMap.add(
        src,
        switch (StateMap.find(src, sfa.transitions)) {
        | exception Not_found => StringMap.singleton(string, dst)
        | string_map =>
          switch (StringMap.find(string, string_map)) {
          | exception Not_found => StringMap.add(string, dst, string_map)
          | cur_dst =>
            raise(
              Non_deterministic(
                "cannot add transition from "
                ++ Int32.to_string(src)
                ++ " for string "
                ++ string
                ++ " to "
                ++ Int32.to_string(dst)
                ++ " due to existing transition to "
                ++ Int32.to_string(cur_dst),
              ),
            )
          }
        },
        sfa.transitions,
      ),
    start: sfa.start,
    finals: sfa.finals,
  };

let count_parents: (state, t) => int =
  (state, sfa) => {
    StateMap.fold(
      (_, string_map, count) =>
        StringMap.fold(
          (_, dst, count) =>
            if (Int32.compare(state, dst) == 0) {
              count + 1;
            } else {
              count;
            },
          string_map,
          count,
        ),
      sfa.transitions,
      0,
    );
  };

let count_children: (state, t) => int =
  (state, sfa) => {
    StateMap.fold(
      (src, string_map, count) =>
        if (Int32.compare(src, state) == 0) {
          StringMap.cardinal(string_map) + count;
        } else {
          count;
        },
      sfa.transitions,
      0,
    );
  };

let group_by: transitions => StateMap.t(StringSetMap.t(state)) =
  transitions =>
    StateMap.fold(
      (src, string_map, acc) =>
        StateMap.fold(
          (dst, string_set, acc) =>
            StateMap.add(
              src,
              switch (StateMap.find(src, acc)) {
              | exception Not_found => StringSetMap.singleton(string_set, dst)
              | char_set_map =>
                StringSetMap.add(string_set, dst, char_set_map)
              },
              acc,
            ),
          StringMap.fold(
            (string, dst, dst_map) =>
              StateMap.add(
                dst,
                switch (StateMap.find(dst, dst_map)) {
                | exception Not_found => StringSet.singleton(string)
                | string_set => StringSet.add(string, string_set)
                },
                dst_map,
              ),
            string_map,
            StateMap.empty,
          ),
          acc,
        ),
      transitions,
      StateMap.empty,
    );

let to_dot: t => string =
  dfa =>
    "digraph {\n"
    ++ "rankdir = LR;\n"
    ++ "node [shape = none; width = 0;] \"\";\n"
    ++ String.concat(
         "\n",
         List.map(
           state => {
             let shape =
               if (StateSet.mem(state, dfa.finals)) {
                 "doublecircle";
               } else {
                 "circle";
               };

             "node [shape = "
             ++ shape
             ++ "] \""
             ++ Int32.to_string(state)
             ++ "\";";
           },
           StateSet.elements(dfa.states),
         ),
       )
    ++ "\n\"\" -> \""
    ++ Int32.to_string(dfa.start)
    ++ "\";\n"
    ++ String.concat(
         "\n",
         List.map(
           ((src, string_set_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((string_set, dst)) =>
                   "\""
                   ++ Int32.to_string(src)
                   ++ "\" -> \""
                   ++ Int32.to_string(dst)
                   ++ "\" [label=\""
                   ++ StringSet.to_string(string_set)
                   ++ "\"];",
                 StringSetMap.bindings(string_set_map),
               ),
             ),
           StateMap.bindings(group_by(dfa.transitions)),
         ),
       )
    ++ "\n}\n";

let to_matrix: t => array(array(string)) =
  dfa => {
    let states = Array.of_list(StateSet.elements(dfa.states));
    let dimx = Array.length(states);
    let grouped_transitions = group_by(dfa.transitions);
    let string_set_set =
      StateMap.fold(
        (_, string_set_map, string_set_set) =>
          StringSetMap.fold(
            (string_set, _, string_set_set) =>
              StringSetSet.add(string_set, string_set_set),
            string_set_map,
            string_set_set,
          ),
        grouped_transitions,
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
              StateMap.find(src, grouped_transitions),
            )
          ) {
          | exception Not_found => ""
          | dst => Int32.to_string(dst)
          }
        );
      };
    };
    matrix;
  };

let accept: (t, string) => bool =
  (dfa, input) => {
    let rec step: (state, list(string)) => bool =
      cur_state =>
        fun
        | [] => StateSet.mem(cur_state, dfa.finals)
        | [cur_string, ...rest] =>
          switch (
            StateMap.find(cur_state, dfa.transitions)
            |> StringMap.find(cur_string)
          ) {
          | exception Not_found => false
          | next_state => step(next_state, rest)
          };

    step(dfa.start, Common.explode_string(input));
  };

let test = () => {
  let nfa =
    singleton(Int32.zero)
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