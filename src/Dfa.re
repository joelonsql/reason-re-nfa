type state = int32;

type transitions = StateMap.t(CharMap.t(state));

exception Non_deterministic(string);

type t = {
  states: StateSet.t,
  alphabet: CharSet.t,
  transitions,
  start: state,
  finals: StateSet.t,
};

let singleton = (start: state) => {
  states: StateSet.singleton(start),
  alphabet: CharSet.empty,
  transitions: StateMap.empty,
  start,
  finals: StateSet.empty,
};

let set_finals = (finals: StateSet.t, dfa) => {
  states: dfa.states,
  alphabet: dfa.alphabet,
  transitions: dfa.transitions,
  start: dfa.start,
  finals:
    StateSet.(
      elements(finals) |> List.map(s => find(s, dfa.states)) |> of_list
    ),
};

let add_transition: ((state, char, state), t) => t =
  ((src, char, dst), dfa) => {
    states: StateSet.(dfa.states |> add(src) |> add(dst)),
    alphabet: CharSet.add(char, dfa.alphabet),
    transitions:
      StateMap.add(
        src,
        switch (StateMap.find(src, dfa.transitions)) {
        | exception Not_found => CharMap.singleton(char, dst)
        | char_map =>
          switch (CharMap.find(char, char_map)) {
          | exception Not_found => CharMap.add(char, dst, char_map)
          | cur_dst =>
            raise(
              Non_deterministic(
                "cannot add transition from "
                ++ Int32.to_string(src)
                ++ " for char "
                ++ String.make(1, char)
                ++ " to "
                ++ Int32.to_string(dst)
                ++ " due to existing transition to "
                ++ Int32.to_string(cur_dst),
              ),
            )
          }
        },
        dfa.transitions,
      ),
    start: dfa.start,
    finals: dfa.finals,
  };

let group_by_charset: transitions => StateMap.t(CharSetMap.t(state)) =
  transitions =>
    StateMap.fold(
      (src, charmap, acc) =>
        StateMap.fold(
          (dst, charset, acc) =>
            StateMap.add(
              src,
              switch (StateMap.find(src, acc)) {
              | exception Not_found => CharSetMap.singleton(charset, dst)
              | char_set_map => CharSetMap.add(charset, dst, char_set_map)
              },
              acc,
            ),
          CharMap.fold(
            (char, dst, dstmap) =>
              StateMap.add(
                dst,
                switch (StateMap.find(dst, dstmap)) {
                | exception Not_found => CharSet.singleton(char)
                | charset => CharSet.add(char, charset)
                },
                dstmap,
              ),
            charmap,
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
             ++ "] "
             ++ Int32.to_string(state)
             ++ ";";
           },
           StateSet.elements(dfa.states),
         ),
       )
    ++ "\n\"\" -> "
    ++ Int32.to_string(dfa.start)
    ++ ";\n"
    ++ String.concat(
         "\n",
         List.map(
           ((src, char_set_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((char_set, dst)) =>
                   Int32.to_string(src)
                   ++ "->"
                   ++ Int32.to_string(dst)
                   ++ " [label=\""
                   ++ CharSet.to_string(char_set)
                   ++ "\"];",
                 CharSetMap.bindings(char_set_map),
               ),
             ),
           StateMap.bindings(group_by_charset(dfa.transitions)),
         ),
       )
    ++ "\n}\n";

let to_c: t => string =
  dfa =>
    "bool match_dfa(char *s) {\n"
    ++ "  bool match = "
    ++ (StateSet.mem(dfa.start, dfa.finals) ? "true" : "false")
    ++ ";\n"
    ++ String.concat(
         "\n",
         List.map(
           src =>
             "  state"
             ++ Int32.to_string(src)
             ++ ":\n"
             ++ "  switch (*s) {\n"
             ++ String.concat(
                  "",
                  List.map(
                    ((charset, dst)) =>
                      String.concat(
                        "\n",
                        List.map(
                          char =>
                            "    case '"
                            ++ Common.escaped_single_quote(char)
                            ++ "':",
                          CharSet.elements(charset),
                        ),
                      )
                      ++ " s++; match = "
                      ++ (StateSet.mem(dst, dfa.finals) ? "true" : "false")
                      ++ "; goto state"
                      ++ Int32.to_string(dst)
                      ++ "; /* "
                      ++ CharSet.to_string(charset)
                      ++ " */\n",
                    switch (
                      StateMap.find(src, group_by_charset(dfa.transitions))
                    ) {
                    | exception Not_found => []
                    | char_set_map => CharSetMap.bindings(char_set_map)
                    },
                  ),
                )
             ++ "    case 0: goto done;\n"
             ++ "    default: match = false; goto done;\n"
             ++ "  }",
           StateSet.elements(dfa.states),
         ),
       )
    ++ "\n"
    ++ "  done:\n"
    ++ "  return match;\n"
    ++ "}\n\n"
    ++ "int main(int argc, char **argv) {\n"
    ++ "  char *s = argv[1];\n"
    ++ "  bool match = match_dfa(s);\n"
    ++ "  return (int)match;\n"
    ++ "}";

let to_llvm_ir: t => string =
  dfa =>
    "define zeroext i1 @match_dfa(i8*) {\n"
    ++ "  %s = alloca i8*, align 8\n"
    ++ "  %match = alloca i8, align 1\n"
    ++ "  store i8* %0, i8** %s, align 8\n"
    ++ "  store i8 "
    ++ (StateSet.mem(dfa.start, dfa.finals) ? "1" : "0")
    ++ ", i8* %match, align 1\n"
    ++ "  br label %state"
    ++ Int32.to_string(dfa.start)
    ++ "\n\n"
    ++ String.concat(
         "\n",
         List.map(
           src => {
             let s = Int32.to_string(src);
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
             ++ ".chr_int, label %miss [\n"
             ++ String.concat(
                  "",
                  List.map(
                    ((char, dst)) =>
                      "    i32 "
                      ++ string_of_int(Char.code(char))
                      ++ ", label %state"
                      ++ s
                      ++ ".goto.state"
                      ++ Int32.to_string(dst)
                      ++ " ; "
                      ++ Common.escaped(char)
                      ++ "\n",
                    switch (StateMap.find(src, dfa.transitions)) {
                    | exception Not_found => []
                    | char_map => CharMap.bindings(char_map)
                    },
                  ),
                )
             ++ "    i32 0, label %done\n"
             ++ "  ]\n";
           },
           StateSet.elements(dfa.states),
         ),
       )
    ++ "\n"
    ++ String.concat(
         "\n",
         List.map(
           ((src, char_set_map)) => {
             let s = Int32.to_string(src);
             String.concat(
               "\n",
               List.map(
                 ((_, dst)) => {
                   let d = Int32.to_string(dst);
                   "state"
                   ++ s
                   ++ ".goto.state"
                   ++ d
                   ++ ":\n"
                   ++ "  store i8* %state"
                   ++ s
                   ++ ".s_next_ptr, i8** %s, align 8\n"
                   ++ "  store i8 "
                   ++ (StateSet.mem(dst, dfa.finals) ? "1" : "0")
                   ++ ", i8* %match, align 1\n"
                   ++ "  br label %state"
                   ++ d
                   ++ "\n";
                 },
                 CharSetMap.bindings(char_set_map),
               ),
             );
           },
           StateMap.bindings(group_by_charset(dfa.transitions)),
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
    let states = Array.of_list(StateSet.elements(dfa.states));
    let dimx = Array.length(states);
    let grouped_transitions = group_by_charset(dfa.transitions);
    let charsetset =
      StateMap.fold(
        (_, char_set_map, charsetset) =>
          CharSetMap.fold(
            (charset, _, charsetset) => CharSetSet.add(charset, charsetset),
            char_set_map,
            charsetset,
          ),
        grouped_transitions,
        CharSetSet.empty,
      );
    let alphabet = Array.of_list(CharSetSet.elements(charsetset));
    let dimy = Array.length(alphabet);
    let matrix = Array.make_matrix(dimx + 1, dimy + 1, "");
    for (x in 1 to dimx) {
      let src = states[x - 1];
      matrix[x][0] = Int32.to_string(src);
      for (y in 1 to dimy) {
        let charset = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = CharSet.to_string(charset);
        };
        matrix[x][y] = (
          switch (
            CharSetMap.find(charset, StateMap.find(src, grouped_transitions))
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
    let rec step: (state, list(char)) => bool =
      cur_state =>
        fun
        | [] => StateSet.mem(cur_state, dfa.finals)
        | [cur_char, ...rest] =>
          switch (
            StateMap.find(cur_state, dfa.transitions)
            |> CharMap.find(cur_char)
          ) {
          | exception Not_found => false
          | next_state => step(next_state, rest)
          };

    step(dfa.start, Common.explode(input));
  };

let test = () => {
  let dfa =
    singleton(Int32.of_int(0))
    |> add_transition((Int32.of_int(0), 'a', Int32.of_int(1)))
    |> add_transition((Int32.of_int(0), 'b', Int32.of_int(1)))
    |> add_transition((Int32.of_int(0), 'c', Int32.of_int(1)))
    |> add_transition((Int32.of_int(1), 'x', Int32.of_int(0)))
    |> add_transition((Int32.of_int(1), 'y', Int32.of_int(0)))
    |> set_finals(StateSet.singleton(Int32.of_int(0)));
  assert(accept(dfa, "ax"));
  assert(!accept(dfa, "axb"));
};

let randomize = () => {
  let rec build = (dfa, src) =>
    fun
    | 10 => dfa
    | n => {
        let dst = Random.int(10);
        let chr = Char.chr(65 + Random.int(26));
        build(
          dfa |> add_transition((Int32.of_int(src), chr, Int32.of_int(dst))),
          dst,
          n + 1,
        );
      };

  let dfa = build(singleton(Int32.of_int(0)), 0, 0);
  let states = Array.of_list(StateSet.elements(dfa.states));
  let dfa =
    set_finals(
      StateSet.of_list([
        states[Random.int(Array.length(states))],
        states[Random.int(Array.length(states))],
        states[Random.int(Array.length(states))],
        states[Random.int(Array.length(states))],
        states[Random.int(Array.length(states))],
      ]),
      dfa,
    );

  dfa;
};