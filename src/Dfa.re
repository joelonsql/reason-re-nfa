type state = StateSet.t;

type transitions = StateSetMap.t(StringMap.t(state));

type grouped_transitions = StateSetMap.t(StringSetMap.t(state));

type transitions_switch = {
  cases: StringSetMap.t(state),
  default: option((StringSet.t, state)),
  length: int,
};

type t = {
  states: StateSetSet.t,
  alphabet: StringSet.t,
  transitions,
  start: state,
  finals: StateSetSet.t,
};

exception Non_deterministic(string);
exception Unexpected_num_chars(string);
exception Bug(string);

let singleton = (start: state) => {
  states: StateSetSet.singleton(start),
  alphabet: StringSet.empty,
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
    StateSetSet.(
      elements(finals) |> List.map(s => find(s, dfa.states)) |> of_list
    ),
};

let add_transition: ((state, string, state), t) => t =
  ((src, string, dst), dfa) => {
    states: StateSetSet.(dfa.states |> add(src) |> add(dst)),
    alphabet: StringSet.add(string, dfa.alphabet),
    transitions:
      StateSetMap.add(
        src,
        switch (StateSetMap.find(src, dfa.transitions)) {
        | exception Not_found => StringMap.singleton(string, dst)
        | string_map =>
          switch (StringMap.find(string, string_map)) {
          | exception Not_found => StringMap.add(string, dst, string_map)
          | cur_dst =>
            raise(
              Non_deterministic(
                "cannot add transition from "
                ++ StateSet.to_identifier(src)
                ++ " for string "
                ++ string
                ++ " to "
                ++ StateSet.to_identifier(dst)
                ++ " due to existing transition to "
                ++ StateSet.to_identifier(cur_dst),
              ),
            )
          }
        },
        dfa.transitions,
      ),
    start: dfa.start,
    finals: dfa.finals,
  };

let count_parents: (state, t) => int =
  (state, dfa) => {
    StateSetMap.fold(
      (_, string_map, count) =>
        StringMap.fold(
          (_, dst, count) =>
            if (StateSet.equal(state, dst)) {
              count + 1;
            } else {
              count;
            },
          string_map,
          count,
        ),
      dfa.transitions,
      0,
    );
  };

let count_children: (state, t) => int =
  (state, dfa) => {
    StateSetMap.fold(
      (src, string_map, count) =>
        if (StateSet.equal(src, state)) {
          StringMap.cardinal(string_map) + count;
        } else {
          count;
        },
      dfa.transitions,
      0,
    );
  };

let group_by: transitions => grouped_transitions =
  transitions =>
    StateSetMap.fold(
      (src, string_map, acc) =>
        StateSetMap.fold(
          (dst, string_set, acc) =>
            StateSetMap.add(
              src,
              switch (StateSetMap.find(src, acc)) {
              | exception Not_found => StringSetMap.singleton(string_set, dst)
              | string_set_map =>
                StringSetMap.add(string_set, dst, string_set_map)
              },
              acc,
            ),
          StringMap.fold(
            (string, dst, dst_map) =>
              StateSetMap.add(
                dst,
                switch (StateSetMap.find(dst, dst_map)) {
                | exception Not_found => StringSet.singleton(string)
                | string_set => StringSet.add(string, string_set)
                },
                dst_map,
              ),
            string_map,
            StateSetMap.empty,
          ),
          acc,
        ),
      transitions,
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
           ((src, string_set_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((string_set, dst)) =>
                   "\""
                   ++ StateSet.to_string(src)
                   ++ "\" -> \""
                   ++ StateSet.to_string(dst)
                   ++ "\" [label=\""
                   ++ StringSet.to_string(string_set)
                   ++ "\"];",
                 StringSetMap.bindings(string_set_map),
               ),
             ),
           StateSetMap.bindings(group_by(dfa.transitions)),
         ),
       )
    ++ "\n}\n";

let to_matrix: t => array(array(string)) =
  dfa => {
    let states = Array.of_list(StateSetSet.elements(dfa.states));
    let dimx = Array.length(states);
    let grouped_transitions = group_by(dfa.transitions);
    let string_set_set =
      StateSetMap.fold(
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
      matrix[x][0] = StateSet.to_string(src);
      for (y in 1 to dimy) {
        let string_set = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = StringSet.to_string(string_set);
        };
        matrix[x][y] = (
          switch (
            StringSetMap.find(
              string_set,
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

let build_transitions_switch:
  grouped_transitions => StateSetMap.t(transitions_switch) =
  state_map =>
    StateSetMap.fold(
      (state_map, string_set_map, state_map') => {
        let (_, _, _, default, length) =
          StringSetMap.fold(
            (
              string_set,
              dst,
              (total, cur_max, cur_max_string_set_dst, default, _),
            ) => {
              let count =
                StringSet.cardinal(
                  StringSet.filter(s => String.length(s) == 1, string_set) /* only count one character strings */
                );
              let total = total + count;
              let (cur_max, cur_max_string_set_dst) =
                if (count > cur_max) {
                  (count, Some((string_set, dst)));
                } else {
                  (cur_max, cur_max_string_set_dst);
                };

              let default =
                if (total == 255) {
                  cur_max_string_set_dst;
                } else {
                  default;
                };

              let length = String.length(StringSet.choose(string_set));

              (total, cur_max, cur_max_string_set_dst, default, length);
            },
            string_set_map,
            (0, 0, None, None, 0),
          );

        StateSetMap.add(
          state_map,
          {
            cases:
              switch (default) {
              | None => string_set_map
              | Some((default_string_set, default_dst)) =>
                StringSetMap.filter(
                  (string_set, dst) =>
                    !(
                      StringSet.equal(string_set, default_string_set)
                      && StateSet.equal(dst, default_dst)
                    ),
                  string_set_map,
                )
              },
            default,
            length,
          },
          state_map',
        );
      },
      state_map,
      StateSetMap.empty,
    );

let to_llvm_ir: t => string =
  dfa => {
    let state_map_transitions_switch =
      build_transitions_switch(group_by(dfa.transitions));
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
         "\n",
         List.map(
           src => {
             let s = StateSet.to_identifier(src);
             "state"
             ++ s
             ++ ":\n"
             ++ (
               switch (StateSetMap.find(src, state_map_transitions_switch)) {
               | exception Not_found => "  br label %detect_end_of_string\n"
               | transitions_switch =>
                 let default_label =
                   switch (transitions_switch.default) {
                   | None =>
                     if (transitions_switch.length == 1) {
                       "miss";
                     } else {
                       "detect_end_of_string";
                     }
                   | Some((_, dst)) =>
                     "state"
                     ++ s
                     ++ ".goto.state"
                     ++ StateSet.to_identifier(dst)
                   };
                 let itype =
                   switch (transitions_switch.length) {
                   | n when n > 8 => "<" ++ string_of_int(n) ++ " x i8>"
                   | n => "i" ++ string_of_int(n * 8)
                   };
                 let len = string_of_int(transitions_switch.length);
                 "  %state"
                 ++ s
                 ++ ".s_ptr = load i8*, i8** %s, align 8\n"
                 ++ "  %state"
                 ++ s
                 ++ ".s_next_ptr = getelementptr inbounds i8, i8* %state"
                 ++ s
                 ++ ".s_ptr, i32 "
                 ++ string_of_int(transitions_switch.length)
                 ++ "\n"
                 ++ (
                   switch (transitions_switch.length) {
                   | 1 =>
                     "  %state"
                     ++ s
                     ++ ".chr = load i8, i8* %state"
                     ++ s
                     ++ ".s_ptr, align 1\n"
                   | n when n >= 2 && n <= 8 =>
                     "  %state"
                     ++ s
                     ++ ".chr_ptr = bitcast i8* %state"
                     ++ s
                     ++ ".s_ptr to "
                     ++ itype
                     ++ "*\n"
                     ++ "  %state"
                     ++ s
                     ++ ".chr = load "
                     ++ itype
                     ++ ", "
                     ++ itype
                     ++ "* %state"
                     ++ s
                     ++ ".chr_ptr, align 1\n"
                   | n when n > 8 =>
                     "  %state"
                     ++ s
                     ++ ".vptr = bitcast i8* %state"
                     ++ s
                     ++ ".s_ptr to "
                     ++ itype
                     ++ "*\n"
                     ++ "  %state"
                     ++ s
                     ++ ".rhs = load "
                     ++ itype
                     ++ ", "
                     ++ itype
                     ++ "* %state"
                     ++ s
                     ++ ".vptr, align 1\n"
                   | n => raise(Unexpected_num_chars(string_of_int(n)))
                   }
                 )
                 ++ (
                   switch (transitions_switch.length) {
                   | n when n >= 1 && n <= 8 =>
                     "  switch "
                     ++ itype
                     ++ " %state"
                     ++ s
                     ++ ".chr, label %"
                     ++ default_label
                     ++ " [\n"
                   | n when n > 8 => ""
                   | n => raise(Unexpected_num_chars(string_of_int(n)))
                   }
                 )
                 ++ String.concat(
                      "",
                      List.map(
                        ((string_set, dst)) => {
                          let d = StateSet.to_identifier(dst);
                          String.concat(
                            "",
                            List.map(
                              string => {
                                if (transitions_switch.length
                                    != String.length(string)) {
                                  print_endline(
                                    "transitions_switch.length: "
                                    ++ string_of_int(
                                         transitions_switch.length,
                                       )
                                    ++ " != String.length(string): "
                                    ++ string_of_int(String.length(string)),
                                  );
                                };
                                switch (transitions_switch.length) {
                                | n when n >= 1 && n <= 8 =>
                                  "    "
                                  ++ itype
                                  ++ " "
                                  ++ Common.encode_string_as_int_or_vector(
                                       string,
                                     )
                                  ++ ", label %state"
                                  ++ s
                                  ++ ".goto.state"
                                  ++ d
                                  ++ " ; "
                                  ++ Common.escape_string(string)
                                  ++ "\n"
                                | n when n > 8 =>
                                  "  %state"
                                  ++ s
                                  ++ ".cmp_mask = icmp eq "
                                  ++ itype
                                  ++ " %state"
                                  ++ s
                                  ++ ".rhs, "
                                  ++ Common.encode_string_as_int_or_vector(
                                       string,
                                     )
                                  ++ " ; "
                                  ++ Common.escape_string(string)
                                  ++ "\n"
                                  ++ "  %state"
                                  ++ s
                                  ++ ".cmp_int = bitcast <"
                                  ++ len
                                  ++ " x i1> %state"
                                  ++ s
                                  ++ ".cmp_mask to i"
                                  ++ len
                                  ++ "\n"
                                  ++ "  %state"
                                  ++ s
                                  ++ ".is_equal = icmp eq i"
                                  ++ len
                                  ++ " %state"
                                  ++ s
                                  ++ ".cmp_int, -1 ; 0b"
                                  ++ String.make(
                                       transitions_switch.length,
                                       '1',
                                     )
                                  ++ "\n"
                                  ++ "  br i1 %state"
                                  ++ s
                                  ++ ".is_equal, label %state"
                                  ++ s
                                  ++ ".goto.state"
                                  ++ d
                                  ++ ", label %detect_end_of_string\n"
                                | n =>
                                  raise(
                                    Unexpected_num_chars(string_of_int(n)),
                                  )
                                };
                              },
                              StringSet.elements(string_set),
                            ),
                          );
                        },
                        StringSetMap.bindings(transitions_switch.cases),
                      ),
                    )
                 ++ (
                   switch (transitions_switch.length) {
                   | n when n == 1 =>
                     "    " ++ itype ++ " 0, label %done\n" ++ "  ]\n"
                   | n when n >= 2 && n <= 8 => "  ]\n"
                   | n when n > 8 => ""

                   | n => raise(Unexpected_num_chars(string_of_int(n)))
                   }
                 );
               }
             );
           },
           StateSetSet.elements(dfa.states),
         ),
       )
    ++ "\n"
    ++ String.concat(
         "\n",
         List.map(
           ((src, string_set_map)) => {
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
                 StringSetMap.bindings(string_set_map),
               ),
             );
           },
           StateSetMap.bindings(group_by(dfa.transitions)),
         ),
       )
    ++ "\n"
    ++ "detect_end_of_string:\n"
    ++ "  %s_ptr = load i8*, i8** %s, align 8\n"
    ++ "  %chr = load i8, i8* %s_ptr, align 1\n"
    ++ "  %chr_is_zero = icmp eq i8 %chr, 0\n"
    ++ "  br i1 %chr_is_zero, label %done, label %miss\n"
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
    ++ "\n"
    ++ "define i32 @main(i32, i8** nocapture readonly) {\n"
    ++ "  %argv = getelementptr inbounds i8*, i8** %1, i64 1\n"
    ++ "  %input_string = load i8*, i8** %argv, align 8\n"
    ++ "  %matched = tail call zeroext i1 @match_dfa(i8* %input_string)\n"
    ++ "  %ret = zext i1 %matched to i32\n"
    ++ "  ret i32 %ret\n"
    ++ "}\n";
  };

let accept: (t, string) => bool =
  (dfa, input) => {
    let rec step: (state, list(string)) => bool =
      cur_state =>
        fun
        | [] => StateSetSet.mem(cur_state, dfa.finals)
        | [cur_string, ...rest] =>
          switch (
            StateSetMap.find(cur_state, dfa.transitions)
            |> StringMap.find(cur_string)
          ) {
          | exception Not_found => false
          | next_state => step(next_state, rest)
          };

    step(dfa.start, Common.explode_string(input));
  };

let test = () => {
  let dfa =
    singleton(StateSet.of_ints([0]))
    |> add_transition((StateSet.of_ints([0]), "a", StateSet.of_ints([1])))
    |> add_transition((StateSet.of_ints([0]), "b", StateSet.of_ints([1])))
    |> add_transition((StateSet.of_ints([0]), "c", StateSet.of_ints([1])))
    |> add_transition((StateSet.of_ints([1]), "x", StateSet.of_ints([0])))
    |> add_transition((StateSet.of_ints([1]), "y", StateSet.of_ints([0])))
    |> set_finals(StateSetSet.singleton(StateSet.of_ints([0])));

  assert(accept(dfa, "ax"));
  assert(!accept(dfa, "axb"));
};