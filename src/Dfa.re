open Common

type state = int32;
type transitions = StateMap.t(CharMap.t(state));
type src_dsts = StateMap.t(StateSet.t);
type t = {
  states: StateSet.t,
  alphabet: CharSet.t,
  transitions,
  start: state,
  finals: StateSet.t,
  src_dsts,
};

let singleton = (start: state) => {
  states: StateSet.singleton(start),
  alphabet: CharSet.empty,
  transitions: StateMap.empty,
  start,
  finals: StateSet.empty,
  src_dsts: StateMap.empty,
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
  src_dsts: dfa.src_dsts,
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
        | char_map => CharMap.add(char, dst, char_map)
        },
        dfa.transitions,
      ),
    start: dfa.start,
    finals: dfa.finals,
    src_dsts: StateMap.add(
      src,
      switch (StateMap.find(src, dfa.src_dsts)) {
      | exception Not_found => StateSet.singleton(dst)
      | dsts => StateSet.add(dst, dsts)
      },
      dfa.src_dsts
    ),
  };

let to_dot: t => string =
  dfa =>
    "digraph {\n" ++
    "rankdir = LR;\n" ++
    "node [shape = none; width = 0;] \"\";\n" ++
    String.concat(
      "\n",
      List.map(
        (state) => {
          let shape = if (StateSet.mem(state, dfa.finals)) {
            "doublecircle"
          } else {
            "circle"
          };
          "node [shape = " ++ shape ++ "] " ++ Int32.to_string(state) ++ ";";
        },
        StateSet.elements(dfa.states),
      ),
    ) ++
    "\n\"\" -> " ++ Int32.to_string(dfa.start) ++ ";\n" ++
    String.concat(
      "\n",
      List.map(
        ((src, char_map)) =>
          String.concat(
            "\n",
            List.map(
              ((char, dst)) =>
                Int32.to_string(src)
                ++ "->"
                ++ Int32.to_string(dst)
                ++ " [label=\""
                ++ String.make(1, char)
                ++ "\"];",
              CharMap.bindings(char_map),
            ),
          ),
        StateMap.bindings(dfa.transitions),
      ),
    ) ++
    "\n}\n";

let to_c: t => string =
  dfa =>
    "bool match_dfa(char *s) {\n" ++
    "  bool match = " ++ (StateSet.mem(dfa.start, dfa.finals) ? "true" : "false") ++ ";\n" ++
    String.concat(
      "\n",
      List.map(
        (src) =>
          "  state" ++ Int32.to_string(src) ++ ":\n" ++
          "  switch (*s) {\n" ++
          String.concat(
            "",
            List.map(
              ((char, dst)) =>
                "    case '" ++ String.make(1, char) ++ "': s++; match = " ++ (StateSet.mem(dst, dfa.finals) ? "true" : "false") ++ "; goto state" ++ Int32.to_string(dst) ++ ";\n",
              switch (StateMap.find(src, dfa.transitions)) {
                | exception Not_found => []
                | char_map => CharMap.bindings(char_map)
              },
            )
          ) ++
          "    case 0: goto done;\n" ++
          "    default: match = false; goto done;\n" ++
          "  }",
        StateSet.elements(dfa.states),
      ),
    ) ++ "\n" ++
    "  done:\n" ++
    "  return match;\n" ++
    "}\n\n" ++
    "int main(int argc, char **argv) {\n" ++
    "  char *s = argv[1];\n" ++
    "  bool match = match_dfa(s);\n" ++
    "  return (int)match;\n" ++
    "}";

let to_llvm_ir: t => string =
  dfa =>
    "define zeroext i1 @match_dfa(i8*) {\n" ++
    "  %s = alloca i8*, align 8\n" ++
    "  %match = alloca i8, align 1\n" ++
    "  store i8* %0, i8** %s, align 8\n" ++
    "  store i8 " ++ (StateSet.mem(dfa.start, dfa.finals) ? "1" : "0") ++ ", i8* %match, align 1\n" ++
    "  br label %state" ++ Int32.to_string(dfa.start) ++ "\n\n" ++
    String.concat(
      "\n",
      List.map(
        (src) => {
          let s = Int32.to_string(src);
          "state" ++ s ++ ":\n" ++
          "  %state" ++ s ++ ".s_ptr = load i8*, i8** %s, align 8\n" ++
          "  %state" ++ s ++ ".chr = load i8, i8* %state" ++ s ++ ".s_ptr, align 1\n" ++
          "  %state" ++ s ++ ".chr_int = sext i8 %state" ++ s ++ ".chr to i32\n" ++
          "  %state" ++ s ++ ".s_next_ptr = getelementptr inbounds i8, i8* %state" ++ s ++ ".s_ptr, i32 1\n" ++
          "  switch i32 %state" ++ s ++ ".chr_int, label %miss [\n" ++
          String.concat(
            "",
            List.map(
              ((char, dst)) =>
                "    i32 " ++ string_of_int(Char.code(char)) ++ ", label %state" ++ s ++ ".goto.state" ++ Int32.to_string(dst) ++ " ; " ++ String.make(1,char) ++ "\n",
              switch (StateMap.find(src, dfa.transitions)) {
                | exception Not_found => []
                | char_map => CharMap.bindings(char_map)
              },
            )
          ) ++
          "    i32 0, label %done\n" ++
          "  ]\n"
        },
        StateSet.elements(dfa.states),
      ),
    ) ++
    "\n" ++
    String.concat(
      "\n",
      List.map(
        ((src, dsts)) => {
          let s = Int32.to_string(src);
          String.concat("\n", List.map(
            (dst) => {
              let d = Int32.to_string(dst);
              "state" ++ s ++ ".goto.state" ++ d ++ ":\n" ++
              "  store i8* %state" ++ s ++ ".s_next_ptr, i8** %s, align 8\n" ++
              "  store i8 " ++ (StateSet.mem(src, dfa.finals) ? "1" : "0") ++ ", i8* %match, align 1\n" ++
              "  br label %state" ++ d ++ "\n"
            },
            StateSet.elements(dsts)
          ))
        },
        StateMap.bindings(dfa.src_dsts),
      ),
    ) ++
    "\n" ++
      "miss:\n" ++
    "  store i8 0, i8* %match, align 1\n" ++
    "  br label %done\n" ++
    "\n" ++
    "done:\n" ++
    "  %match_val = load i8, i8* %match, align 1\n" ++
    "  %ret = trunc i8 %match_val to i1\n" ++
    "  ret i1 %ret\n" ++
    "}\n" ++
    "\n" ++
    "define i32 @main(i32, i8**) {\n" ++
    "  %argv = getelementptr inbounds i8*, i8** %1, i64 1\n" ++
    "  %s = load i8*, i8** %argv, align 8\n" ++
    "  %match = tail call zeroext i1 @match_dfa(i8* %s)\n" ++
    "  %ret = zext i1 %match to i32\n" ++
    "  ret i32 %ret\n" ++
    "}\n"
;

let to_matrix: t => array(array(string)) =
  dfa => {
    let states = Array.of_list(StateSet.elements(dfa.states));
    let dimx = Array.length(states);
    let alphabet = Array.of_list(CharSet.elements(dfa.alphabet));
    let dimy = Array.length(alphabet);
    let matrix = Array.make_matrix(dimx + 1, dimy + 1, "");
    for (x in 1 to dimx) {
      let src = states[x - 1];
      matrix[x][0] = Int32.to_string(src);
      for (y in 1 to dimy) {
        let char = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = String.make(1, char);
        };
        matrix[x][y] = (
          switch (CharMap.find(char, StateMap.find(src, dfa.transitions))) {
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

    step(dfa.start, explode(input));
  };

let dump = (dfa) => {
  Js.log("Dfa.states: " ++ StateSet.to_string(dfa.states));
  Js.log("Dfa.alphabet: " ++ CharSet.to_string(dfa.alphabet));
  Js.log("Dfa.transitions:");
  Js.log(to_matrix(dfa));
  Js.log("Dfa.start: " ++ Int32.to_string(dfa.start));
  Js.log("Dfa.finals: " ++ StateSet.to_string(dfa.finals));
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
  Js.log(to_llvm_ir(dfa));
};

let randomize = () => {
  let rec build = (dfa,src) => fun
    | 10 => dfa
    | n => {
      let dst = Random.int(10);
      let chr = Char.chr(65+Random.int(26));
      build(dfa |> add_transition((Int32.of_int(src), chr, Int32.of_int(dst))), dst, n+1)
    }
  ;
  let dfa = build(singleton(Int32.of_int(0)), 0, 0)
  let states = Array.of_list(StateSet.elements(dfa.states));
  let dfa = set_finals(
    StateSet.of_list([
      Array.get(states, Random.int(Array.length(states))),
      Array.get(states, Random.int(Array.length(states))),
      Array.get(states, Random.int(Array.length(states))),
      Array.get(states, Random.int(Array.length(states))),
      Array.get(states, Random.int(Array.length(states))),
    ]),
    dfa
  );
  Js.log(to_llvm_ir(dfa));
};
