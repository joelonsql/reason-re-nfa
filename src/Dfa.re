type state = StateSet.t;

type transitions = StateSetMap.t(RangeSetListSetMap.t(state));

type t = {
  states: StateSetSet.t,
  alphabet: RangeSetListSetSet.t,
  transitions,
  start: state,
  finals: StateSetSet.t,
};

type inline =
  | Never
  | SingleEntry
  | Always;

exception Non_deterministic(string);
exception Unexpected_num_chars(string);
exception Bug(string);

let singleton = (start: state) => {
  states: StateSetSet.singleton(start),
  alphabet: RangeSetListSetSet.empty,
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

let exists_transition: ((state, RangeSetListSet.t, state), t) => bool =
  ((src, ranges, dst), dfa) => {
    switch (
      StateSetMap.find(src, dfa.transitions)
      |> RangeSetListSetMap.find(ranges)
    ) {
    | exception Not_found => false
    | dst' => StateSet.equal(dst, dst')
    };
  };

let add_transition: ((state, RangeSetListSet.t, state), t) => t =
  ((src, ranges, dst), dfa) => {
    states: StateSetSet.(dfa.states |> add(src) |> add(dst)),
    alphabet: RangeSetListSetSet.add(ranges, dfa.alphabet),
    transitions:
      StateSetMap.add(
        src,
        switch (StateSetMap.find(src, dfa.transitions)) {
        | exception Not_found => RangeSetListSetMap.singleton(ranges, dst)
        | ranges_map =>
          switch (RangeSetListSetMap.find(ranges, ranges_map)) {
          | exception Not_found =>
            RangeSetListSetMap.add(ranges, dst, ranges_map)
          | cur_dst =>
            raise(
              Non_deterministic(
                "cannot add transition from "
                ++ StateSet.to_identifier(src)
                ++ " for ranges "
                ++ RangeSetListSet.to_string(ranges)
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
    let (_, count) =
      StateSetMap.fold(
        (src, ranges_map, (srcs, count)) =>
          RangeSetListSetMap.fold(
            (_, dst, (srcs, count)) =>
              if (StateSet.equal(state, dst) && !StateSetSet.mem(src, srcs)) {
                (StateSetSet.add(src, srcs), count + 1);
              } else {
                (srcs, count);
              },
            ranges_map,
            (srcs, count),
          ),
        dfa.transitions,
        (StateSetSet.empty, 0),
      );
    count;
  };

let count_children: (state, t) => int =
  (state, dfa) => {
    StateSetMap.fold(
      (src, ranges_map, count) =>
        if (StateSet.equal(src, state)) {
          RangeSetListSetMap.count_strings(ranges_map) + count;
        } else {
          count;
        },
      dfa.transitions,
      0,
    );
  };

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
           ((src, ranges_map)) =>
             String.concat(
               "\n",
               List.map(
                 ((ranges, dst)) =>
                   "\""
                   ++ StateSet.to_string(src)
                   ++ "\" -> \""
                   ++ StateSet.to_string(dst)
                   ++ "\" [label=\""
                   ++ RangeSetListSet.to_string(ranges)
                   ++ "\"];",
                 RangeSetListSetMap.bindings(ranges_map),
               ),
             ),
           StateSetMap.bindings(dfa.transitions),
         ),
       )
    ++ "\n}\n";

let to_matrix: t => array(array(string)) =
  dfa => {
    let states = Array.of_list(StateSetSet.elements(dfa.states));
    let dimx = Array.length(states);
    let ranges_set =
      StateSetMap.fold(
        (_, ranges_map, ranges_set) =>
          RangeSetListSetMap.fold(
            (ranges, _, ranges_set) =>
              RangeSetListSetSet.add(ranges, ranges_set),
            ranges_map,
            ranges_set,
          ),
        dfa.transitions,
        RangeSetListSetSet.empty,
      );

    let alphabet = Array.of_list(RangeSetListSetSet.elements(ranges_set));
    let dimy = Array.length(alphabet);
    let matrix = Array.make_matrix(dimx + 1, dimy + 1, "");
    for (x in 1 to dimx) {
      let src = states[x - 1];
      matrix[x][0] = StateSet.to_string(src);
      for (y in 1 to dimy) {
        let ranges = alphabet[y - 1];
        if (x == 1) {
          matrix[0][y] = RangeSetListSet.to_string(ranges);
        };
        matrix[x][y] = (
          switch (
            RangeSetListSetMap.find(
              ranges,
              StateSetMap.find(src, dfa.transitions),
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

let code_gen = (target_language, inline, dfa) => {
  let rec build_state:
    (state, StateSetSet.t, option(bool), list(state)) =>
    (string, StateSetSet.t) =
    (src, todo, cur_in_accepting_state, srcs) => {
      let src_state = Int32.to_string(StateSet.choose_strict(src));
      if (!StateSetMap.mem(src, dfa.transitions)) {
        (CodeGen.break_loop_switch(target_language), todo);
      } else {
        let string_map = StateSetMap.find(src, dfa.transitions);
        let (strings, _) = RangeSetListSetMap.choose(string_map);
        let str_len = List.length(RangeSetListSet.choose(strings));
        let (cases, todo) =
          RangeSetListSetMap.fold(
            (string_set, dst, (cases, todo)) => {
              let dst_state = Int32.to_string(StateSet.choose_strict(dst));
              RangeSetListSet.iter(
                string => assert(List.length(string) == str_len),
                string_set,
              );
              let (_, cases_to_same_dst) =
                List.fold_right(
                  (string, (i, acc)) => {
                    assert(String.length(string) == str_len);
                    (
                      succ(i),
                      [
                        CodeGen.switch_case_value(
                          target_language,
                          string_of_int(i),
                          str_len,
                          string,
                          src_state,
                          dst_state,
                        ),
                        ...acc,
                      ],
                    );
                  },
                  List.rev(RangeSetListSet.generate_strings(string_set)),
                  (0, []),
                );

              let in_accepting_state = StateSetSet.mem(dst, dfa.finals);
              let single_entry = count_parents(dst, dfa) == 1;
              let branch_to_previous = List.mem(dst, srcs);
              let (inline_or_branch, todo) =
                switch (branch_to_previous, inline, single_entry) {
                /*** (A) Branching back to previous state */
                | (true, _, _) => (
                    CodeGen.continue(target_language, dst_state),
                    todo,
                  )

                /*** (B) Inline state if single-entry
                     or if we always want to inline
                     to reduce jumps instead of
                     reducing code size */
                | (false, SingleEntry, true)
                | (false, Always, _) =>
                  let (code, todo) =
                    build_state(
                      dst,
                      todo,
                      Some(in_accepting_state),
                      [dst, ...srcs],
                    );
                  if (count_parents(dst, dfa) > 1) {
                    (
                      CodeGen.labeled_block(target_language, dst_state, code),
                      todo,
                    );
                  } else {
                    (code, todo);
                  };

                /*** (C) Simulate goto if we never want to inline
                     or if we allow inlining single-entry states
                     but this state is a multi-entry state.
                      */
                | (false, Never, _)
                | (false, SingleEntry, false) => (
                    CodeGen.goto_irreducible_state(
                      target_language,
                      dst_state,
                    ),
                    StateSetSet.add(dst, todo),
                  )
                };

              (
                [
                  (
                    String.concat("", List.rev(cases_to_same_dst)),
                    CodeGen.switch_case_code(
                      target_language,
                      str_len,
                      src_state,
                      dst_state,
                      cur_in_accepting_state,
                      in_accepting_state,
                      inline_or_branch,
                    ),
                  ),
                  ...cases,
                ],
                todo,
              );
            },
            string_map,
            ([], todo),
          );
        (
          CodeGen.make_switch(
            target_language,
            src_state,
            str_len,
            List.rev(cases),
          ),
          todo,
        );
      };
    };
  let rec work:
    (StateSetSet.t, StateSetMap.t(string)) =>
    (StateSetSet.t, StateSetMap.t(string)) =
    (todo, states) =>
      if (StateSetSet.is_empty(todo)) {
        (todo, states);
      } else {
        StateSetSet.fold(
          (src, (todo, states)) =>
            if (StateSetMap.mem(src, states)) {
              (todo, states);
            } else {
              let (code, todo') =
                build_state(src, StateSetSet.empty, None, [src]);
              let states = StateSetMap.add(src, code, states);
              let processed =
                StateSetSet.of_list(
                  List.map(
                    ((state, _)) => state,
                    StateSetMap.bindings(states),
                  ),
                );
              let todo =
                StateSetSet.diff(StateSetSet.union(todo, todo'), processed);
              work(todo, states);
            },
          todo,
          (StateSetSet.empty, states),
        );
      };
  let (_, states) =
    work(StateSetSet.singleton(dfa.start), StateSetMap.empty);

  let states_and_code =
    List.map(
      ((src, code)) => {
        let state = Int32.to_int(StateSet.choose_strict(src));
        (state, code);
      },
      StateSetMap.bindings(states),
    );
  let states = CodeGen.switch_case_state(target_language, states_and_code);
  let accept_empty = StateSetSet.mem(dfa.start, dfa.finals);
  let start_state = Int32.to_string(StateSet.choose_strict(dfa.start));

  CodeGen.match_dfa(target_language, accept_empty, start_state, states);
};

let to_js = (inline, dfa) => {
  code_gen(CodeGen.JavaScript, inline, dfa);
};

let to_llvm_ir = (inline, dfa) => {
  code_gen(CodeGen.LLVMIR, inline, dfa);
};

let to_wasm = (inline, dfa) => {
  code_gen(CodeGen.WebAssembly, inline, dfa);
};

let accept: (t, string) => bool =
  (dfa, input) => {
    let rec step: (state, list(RangeSetListSet.t)) => bool =
      cur_state =>
        fun
        | [] => StateSetSet.mem(cur_state, dfa.finals)
        | [cur_string, ...rest] =>
          switch (
            StateSetMap.find(cur_state, dfa.transitions)
            |> RangeSetListSetMap.find(cur_string)
          ) {
          | exception Not_found => false
          | next_state => step(next_state, rest)
          };

    step(dfa.start, RangeSetListSet.explode(input));
  };

let test = () => {
  let dfa =
    singleton(StateSet.of_ints([0]))
    |> add_transition((
         StateSet.of_ints([0]),
         RangeSetListSet.of_char('a'),
         StateSet.of_ints([1]),
       ))
    |> add_transition((
         StateSet.of_ints([0]),
         RangeSetListSet.of_char('b'),
         StateSet.of_ints([1]),
       ))
    |> add_transition((
         StateSet.of_ints([0]),
         RangeSetListSet.of_char('c'),
         StateSet.of_ints([1]),
       ))
    |> add_transition((
         StateSet.of_ints([1]),
         RangeSetListSet.of_char('x'),
         StateSet.of_ints([0]),
       ))
    |> add_transition((
         StateSet.of_ints([1]),
         RangeSetListSet.of_char('y'),
         StateSet.of_ints([0]),
       ))
    |> set_finals(StateSetSet.singleton(StateSet.of_ints([0])));

  assert(accept(dfa, "ax"));
  assert(!accept(dfa, "axb"));
};