type state = StateSet.t;

type transitions = StateSetMap.t(CharMap.t(state));

type grouped_transitions = StateSetMap.t(CharSetMap.t(state));

type transitions_switch = {
  cases: CharSetMap.t(state),
  default: option((CharSet.t, state)),
};

exception Non_deterministic(string);

type t = {
  states: StateSetSet.t,
  alphabet: CharSet.t,
  transitions,
  start: state,
  finals: StateSetSet.t,
};

let singleton = (start: state) => {
  states: StateSetSet.singleton(start),
  alphabet: CharSet.empty,
  transitions: StateSetMap.empty,
  start,
  finals: StateSetSet.empty,
};

let set_finals = (finals: StateSetSet.t, dfa) => {
  states: dfa.states,
  alphabet: dfa.alphabet,
  transitions: dfa.transitions,
  start: dfa.start,
  finals:
    /* Check all states are valid i.e. exists,
       find will throw Not_found otherwise. */
    StateSetSet.(
      elements(finals) |> List.map(s => find(s, dfa.states)) |> of_list
    ),
};

let add_transition: ((state, char, state), t) => t =
  ((src, char, dst), dfa) => {
    states: StateSetSet.(dfa.states |> add(src) |> add(dst)),
    alphabet: CharSet.add(char, dfa.alphabet),
    transitions:
      StateSetMap.add(
        src,
        switch (StateSetMap.find(src, dfa.transitions)) {
        | exception Not_found => CharMap.singleton(char, dst)
        | char_map =>
          switch (CharMap.find(char, char_map)) {
          | exception Not_found => CharMap.add(char, dst, char_map)
          | cur_dst =>
            raise(
              Non_deterministic(
                "cannot add transition from "
                ++ StateSet.to_string(src)
                ++ " for char "
                ++ String.make(1, char)
                ++ " to "
                ++ StateSet.to_string(dst)
                ++ " due to existing transition to "
                ++ StateSet.to_string(cur_dst),
              ),
            )
          }
        },
        dfa.transitions,
      ),
    start: dfa.start,
    finals: dfa.finals,
  };

let group_by: transitions => grouped_transitions =
  transitions =>
    StateSetMap.fold(
      (src, char_map, acc) =>
        StateSetMap.fold(
          (dst, char_set, acc) =>
            StateSetMap.add(
              src,
              switch (StateSetMap.find(src, acc)) {
              | exception Not_found => CharSetMap.singleton(char_set, dst)
              | char_set_map => CharSetMap.add(char_set, dst, char_set_map)
              },
              acc,
            ),
          CharMap.fold(
            (char, dst, dst_map) =>
              StateSetMap.add(
                dst,
                switch (StateSetMap.find(dst, dst_map)) {
                | exception Not_found => CharSet.singleton(char)
                | char_set => CharSet.add(char, char_set)
                },
                dst_map,
              ),
            char_map,
            StateSetMap.empty,
          ),
          acc,
        ),
      transitions,
      StateSetMap.empty,
    );

let build_transitions_switch:
  grouped_transitions => StateSetMap.t(transitions_switch) =
  state_set_map =>
    StateSetMap.fold(
      (state_set, char_set_map, state_set_map') => {
        let (_, _, _, default) =
          CharSetMap.fold(
            (char_set, dst, (total, cur_max, cur_max_char_set_dst, default)) => {
              let count = CharSet.cardinal(char_set);
              let total = total + count;
              let (cur_max, cur_max_char_set_dst) =
                if (count > cur_max) {
                  (count, Some((char_set, dst)));
                } else {
                  (cur_max, cur_max_char_set_dst);
                };

              let default =
                if (total == 255) {
                  cur_max_char_set_dst;
                } else {
                  default;
                };

              (total, cur_max, cur_max_char_set_dst, default);
            },
            char_set_map,
            (0, 0, None, None),
          );

        StateSetMap.add(
          state_set,
          {
            cases:
              switch (default) {
              | None => char_set_map
              | Some((default_char_set, default_dst)) =>
                CharSetMap.filter(
                  (char_set, dst) =>
                    !(
                      CharSet.equal(char_set, default_char_set)
                      && StateSet.equal(dst, default_dst)
                    ),
                  char_set_map,
                )
              },
            default,
          },
          state_set_map',
        );
      },
      state_set_map,
      StateSetMap.empty,
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
               if (StateSetSet.mem(state, dfa.finals)) {
                 "doublecircle";
               } else {
                 "circle";
               };

             "node [shape = "
             ++ shape
             ++ "] \""
             ++ StateSet.to_string(state)
             ++ "\";";
           },
           StateSetSet.elements(dfa.states),
         ),
       )
    ++ "\n\"\" -> \""
    ++ StateSet.to_string(dfa.start)
    ++ "\";\n"
    ++ String.concat(
         "\n",
         List.map(
           ((src, char_set_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((char_set, dst)) =>
                   "\""
                   ++ StateSet.to_string(src)
                   ++ "\" -> \""
                   ++ StateSet.to_string(dst)
                   ++ "\" [label=\""
                   ++ CharSet.to_string(char_set)
                   ++ "\"];",
                 CharSetMap.bindings(char_set_map),
               ),
             ),
           StateSetMap.bindings(group_by(dfa.transitions)),
         ),
       )
    ++ "\n}\n";

let to_llvm_ir: t => string =
  dfa =>
    "define zeroext i1 @match_dfa(i8*) {\n"
    ++ "  %s = alloca i8*, align 8\n"
    ++ "  %match = alloca i8, align 1\n"
    ++ "  store i8* %0, i8** %s, align 8\n"
    ++ "  store i8 "
    ++ (StateSetSet.mem(dfa.start, dfa.finals) ? "1" : "0")
    ++ ", i8* %match, align 1\n"
    ++ "  br label %state"
    ++ StateSet.to_identifier(dfa.start)
    ++ "\n\n"
    ++ String.concat(
         "",
         List.map(
           src => {
             let s = StateSet.to_identifier(src);
             "state"
             ++ s
             ++ ":\n"
             ++ "  %state"
             ++ s
             ++ ".s_ptr = load i8*, i8** %s, align 8\n"
             ++ "  %state"
             ++ s
             ++ ".chr = load i8, i8* %state"
             ++ s
             ++ ".s_ptr, align 1\n"
             ++ "  %state"
             ++ s
             ++ ".chr_int = sext i8 %state"
             ++ s
             ++ ".chr to i32\n"
             ++ "  %state"
             ++ s
             ++ ".s_next_ptr = getelementptr inbounds i8, i8* %state"
             ++ s
             ++ ".s_ptr, i32 1\n"
             ++ "  switch i32 %state"
             ++ s
             ++ ".chr_int, "
             ++ (
               switch (
                 StateSetMap.find(
                   src,
                   build_transitions_switch(group_by(dfa.transitions)),
                 )
               ) {
               | exception Not_found => "label %miss [\n"
               | transitions_switch =>
                 (
                   switch (transitions_switch.default) {
                   | None => "label %miss [\n"
                   | Some((char_set, dst)) =>
                     "label %state"
                     ++ s
                     ++ ".goto.state"
                     ++ StateSet.to_identifier(dst)
                     ++ " [ ; "
                     ++ CharSet.to_string(char_set)
                     ++ "\n"
                   }
                 )
                 ++ String.concat(
                      "\n",
                      List.map(
                        ((char_set, dst)) =>
                          String.concat(
                            "\n",
                            List.map(
                              char =>
                                "    i32 "
                                ++ string_of_int(Char.code(char))
                                ++ ", label %state"
                                ++ s
                                ++ ".goto.state"
                                ++ StateSet.to_identifier(dst)
                                ++ " ; \""
                                ++ Common.escaped(char)
                                ++ "\"\n",
                              CharSet.elements(char_set),
                            ),
                          ),
                        CharSetMap.bindings(transitions_switch.cases),
                      ),
                    )
               }
             )
             ++ "    i32 0, label %done\n"
             ++ "  ]\n";
           },
           StateSetSet.elements(dfa.states),
         ),
       )
    ++ "\n"
    ++ String.concat(
         "\n",
         List.map(
           ((src, char_set_map)) => {
             let s = StateSet.to_identifier(src);
             String.concat(
               "\n",
               List.map(
                 ((_, dst)) => {
                   let d = StateSet.to_identifier(dst);
                   "state"
                   ++ s
                   ++ ".goto.state"
                   ++ d
                   ++ ":\n"
                   ++ "  store i8* %state"
                   ++ s
                   ++ ".s_next_ptr, i8** %s, align 8\n"
                   ++ "  store i8 "
                   ++ (StateSetSet.mem(dst, dfa.finals) ? "1" : "0")
                   ++ ", i8* %match, align 1\n"
                   ++ "  br label %state"
                   ++ d
                   ++ "\n";
                 },
                 CharSetMap.bindings(char_set_map),
               ),
             );
           },
           StateSetMap.bindings(group_by(dfa.transitions)),
         ),
       )
    ++ "\n"
    ++ "miss:\n"
    ++ "  store i8 0, i8* %match, align 1\n"
    ++ "  br label %done\n"
    ++ "\n"
    ++ "done:\n"
    ++ "  %match_val = load i8, i8* %match, align 1\n"
    ++ "  %ret = trunc i8 %match_val to i1\n"
    ++ "  ret i1 %ret\n"
    ++ "}\n"
    ++ "\n";

let to_matrix: t => array(array(string)) =
  dfa => {
    let states = Array.of_list(StateSetSet.elements(dfa.states));
    let dimx = Array.length(states);
    let grouped_transitions = group_by(dfa.transitions);
    let char_set_set =
      StateSetMap.fold(
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
      matrix[x][0] = StateSet.to_string(src);
      for (y in 1 to dimy) {
        let char_set = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = CharSet.to_string(char_set);
        };
        matrix[x][y] = (
          switch (
            CharSetMap.find(
              char_set,
              StateSetMap.find(src, grouped_transitions),
            )
          ) {
          | exception Not_found => ""
          | dst => StateSet.to_string(dst)
          }
        );
      };
    };
    matrix;
  };

let accept: (t, string) => bool =
  (dfa, input) => {
    let rec step: (state, list(char)) => bool =
      cur_state =>
        fun
        | [] => StateSetSet.mem(cur_state, dfa.finals)
        | [cur_char, ...rest] =>
          switch (
            StateSetMap.find(cur_state, dfa.transitions)
            |> CharMap.find(cur_char)
          ) {
          | exception Not_found => false
          | next_state => step(next_state, rest)
          };

    step(dfa.start, Common.explode(input));
  };

let test = () => {
  let dfa =
    singleton(StateSet.of_ints([0]))
    |> add_transition((StateSet.of_ints([0]), 'a', StateSet.of_ints([1])))
    |> add_transition((StateSet.of_ints([0]), 'b', StateSet.of_ints([1])))
    |> add_transition((StateSet.of_ints([0]), 'c', StateSet.of_ints([1])))
    |> add_transition((StateSet.of_ints([1]), 'x', StateSet.of_ints([0])))
    |> add_transition((StateSet.of_ints([1]), 'y', StateSet.of_ints([0])))
    |> set_finals(StateSetSet.singleton(StateSet.of_ints([0])));

  assert(accept(dfa, "ax"));
  assert(!accept(dfa, "axb"));
};