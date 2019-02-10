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

let ll_states = (src_state, switch_state, goto_dsts) => {j|
$src_state:
  %$src_state.s_ptr = load i8*, i8** %s, align 8
  $switch_state
  $goto_dsts
|j};

let ll_load = (str_len, itype, src_state) =>
  switch (str_len) {
  | 1 => {j|
  %$src_state.chr = load i8, i8* %$src_state.s_ptr, align 1
  %$src_state.next_ptr = getelementptr inbounds i8, i8* %$src_state.s_ptr, i32 $str_len
    |j}
  | n when n > 1 && n <= 8 => {j|
  %$src_state.chr_ptr = bitcast i8* %$src_state.s_ptr to $itype*
  %$src_state.chr = load $itype, $itype* %$src_state.chr_ptr, align 1
  %$src_state.next_ptr = getelementptr inbounds i8, i8* %$src_state.s_ptr, i32 $str_len
    |j}
  | n when n > 8 => {j|
  %$src_state.vptr = bitcast i8* %$src_state.s_ptr to $itype*
  %$src_state.rhs = load $itype, $itype* %$src_state.vptr, align 1
  %$src_state.next_ptr = getelementptr inbounds i8, i8* %$src_state.s_ptr, i32 $str_len
    |j}
  | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
  };

let ll_switch_case =
    (i, str_len, itype, ival, src_state, dst_state, string_escaped, bits) =>
  switch (str_len) {
  | n when n >= 1 && n <= 8 => {j|
    $itype $ival, label %$src_state.goto.$dst_state ; $string_escaped
    |j}
  | n when n > 8 => {j|
  %$src_state.$i.cmp_mask = icmp eq $itype %$src_state.rhs, $ival ; $string_escaped
  %$src_state.$i.cmp_int = bitcast <$str_len x i1> %$src_state.$i.cmp_mask to i$str_len
  %$src_state.$i.is_equal = icmp eq i$str_len %$src_state.$i.cmp_int, -1 ; 0b$bits
  br i1 %$src_state.$i.is_equal, label %$src_state.goto.$dst_state, label %$src_state.$i.try_next
$src_state.$i.try_next:
    |j}
  | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
  };

let ll_goto_state = (src_state, dst_state, in_accepting_state) => {j|
$src_state.goto.$dst_state:
  store i8* %$src_state.next_ptr, i8** %s, align 8
  store i8 $in_accepting_state, i8* %match, align 1
  br label %$dst_state
    |j};

let state_goto_handlers = goto_dsts => {
  String.concat(
    "",
    List.map(((_, llvmir)) => llvmir, StateSetMap.bindings(goto_dsts)),
  );
};

/*

  let to_llvm_ir: t => string =
    dfa => {
      let transitions = group_by_str_len(dfa.transitions);
      let accept_empty = StateSetSet.mem(dfa.start, dfa.finals) ? "1" : "0";
      let start_state = StateSet.to_identifier(dfa.start);
      let match_dfa_body =
        String.concat(
          "",
          List.map(
            src => {
              let src_state = StateSet.to_identifier(src);
              let (switch_states, list_goto_dsts) =
                StrLenMap.fold(
                  (str_len, ranges_map, (switch_states, list_goto_dsts)) => {
                    let str_len = Int32.to_int(str_len);
                    let itype =
                      if (str_len > 8) {
                        "<" ++ string_of_int(str_len) ++ " x i8>";
                      } else {
                        "i" ++ string_of_int(str_len * 8);
                      };
                    let (_, cases, goto_dsts) =
                      RangeSetListSetMap.fold(
                        (string, dst, (i, acc, goto_dsts)) => {
                          assert(List.length(string) == str_len);
                          let dst_state = StateSet.to_identifier(dst);
                          let ival =
                            Common.encode_string_as_int_or_vector(string);
                          let string_escaped = Common.escape_string(string);
                          let bits = String.make(str_len, '1');
                          (
                            i + 1,
                            [
                              ll_switch_case(
                                string_of_int(i),
                                str_len,
                                itype,
                                ival,
                                src_state,
                                dst_state,
                                string_escaped,
                                bits,
                              ),
                              ...acc,
                            ],
                            if (StateSetMap.mem(dst, goto_dsts)) {
                              goto_dsts;
                            } else {
                              let in_accepting_state =
                                StateSetSet.mem(dst, dfa.finals) ? "1" : "0";
                              StateSetMap.add(
                                dst,
                                ll_goto_state(
                                  src_state,
                                  dst_state,
                                  in_accepting_state,
                                ),
                                goto_dsts,
                              );
                            },
                          );
                        },
                        ranges_map,
                        (0, [], StateSetMap.empty),
                      );
                    (
                      [
                        ll_load(str_len, itype, src_state)
                        ++ ll_switch(
                             str_len,
                             String.concat("", List.rev(cases)),
                             itype,
                             src_state,
                           ),
                        ...switch_states,
                      ],
                      [state_goto_handlers(goto_dsts), ...list_goto_dsts],
                    );
                  },
                  try (StateSetMap.find(src, transitions)) {
                  | Not_found => StrLenMap.empty
                  },
                  ([], []),
                );
              ll_states(
                src_state,
                String.concat("", switch_states),
                String.concat("", list_goto_dsts),
              );
            },
            StateSetSet.elements(dfa.states),
          ),
        );
      ll_match_dfa(accept_empty, start_state, match_dfa_body);
    };

 */

let to_js = (inline, dfa) => {
  let rec build_state:
    (state, StateSetSet.t, option(bool), list(state)) =>
    (string, StateSetSet.t) =
    (src, todo, cur_in_accepting_state, srcs) => {
      let src_state = Int32.to_string(StateSet.choose_strict(src));
      if (!StateSetMap.mem(src, dfa.transitions)) {
        (CodeGen.JavaScript.break_loop_switch, todo);
      } else {
        let string_map = StateSetMap.find(src, dfa.transitions);
        let (strings, _) = RangeSetListSetMap.choose(string_map);
        let str_len = List.length(RangeSetListSet.choose(strings));
        let (cases, todo) =
          RangeSetListSetMap.fold(
            (string_set, dst, (cases, todo)) => {
              RangeSetListSet.iter(
                string => assert(List.length(string) == str_len),
                string_set,
              );
              let cases_to_same_dst =
                List.fold_right(
                  (string, acc) => {
                    let (ival, string_escaped) =
                      str_len == 1 ?
                        (
                          string_of_int(Char.code(string.[0])),
                          Common.escape_string(string),
                        ) :
                        ({j|"$string"|j}, "");
                    [
                      CodeGen.JavaScript.switch_case_value(
                        ival,
                        string_escaped,
                      ),
                      ...acc,
                    ];
                  },
                  List.rev(RangeSetListSet.generate_strings(string_set)),
                  [],
                );

              let dst_state = Int32.to_string(StateSet.choose_strict(dst));
              let in_accepting_state = StateSetSet.mem(dst, dfa.finals);
              let single_entry = count_parents(dst, dfa) == 1;
              let branch_to_previous = List.mem(dst, srcs);
              let (inline_or_branch, todo) =
                switch (branch_to_previous, inline, single_entry) {
                /*** (A) Branching back to previous state */
                | (true, _, _) => (
                    CodeGen.JavaScript.continue(dst_state),
                    todo,
                  )

                /*** (B) Inline state if single-entry
                     or if we always want to inline
                     to reduce jumps instead of
                     reducing code size */
                | (false, SingleEntry, true)
                | (false, Always, _) =>
                  build_state(
                    dst,
                    todo,
                    Some(in_accepting_state),
                    [dst, ...srcs],
                  )

                /*** (C) Simulate goto if we never want to inline
                     or if we allow inlining single-entry states
                     but this state is a multi-entry state.
                      */
                | (false, Never, _)
                | (false, SingleEntry, false) => (
                    CodeGen.JavaScript.goto_irreducible_state(dst_state),
                    StateSetSet.add(dst, todo),
                  )
                };

              let set_match =
                switch (cur_in_accepting_state) {
                | Some(m) when m == in_accepting_state => "" /* match variable already has correct value from previous state */
                | _ => in_accepting_state ? "match = true;" : "match = false;"
                };

              (
                [
                  String.concat("", List.rev(cases_to_same_dst))
                  ++ CodeGen.JavaScript.switch_case_code(
                       str_len,
                       src_state,
                       dst_state,
                       set_match,
                       inline_or_branch,
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
          if (count_parents(src, dfa) > 1 || StateSet.equal(src, dfa.start)) {
            CodeGen.JavaScript.labeled_block(
              src_state,
              CodeGen.JavaScript.make_switch(
                src_state,
                str_len,
                String.concat("", List.rev(cases)),
              ),
            );
          } else {
            CodeGen.JavaScript.make_switch(
              src_state,
              str_len,
              String.concat("", List.rev(cases)),
            );
          },
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
              let src_state = Int32.to_string(StateSet.choose_strict(src));
              let states =
                StateSetMap.add(
                  src,
                  CodeGen.JavaScript.switch_case_state(src_state, code),
                  states,
                );
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

  let states =
    String.concat(
      "",
      List.map(((_, code)) => code, StateSetMap.bindings(states)),
    );
  let accept_empty =
    StateSetSet.mem(dfa.start, dfa.finals) ? "true" : "false";
  let start_state = Int32.to_string(StateSet.choose_strict(dfa.start));

  CodeGen.JavaScript.match_dfa(accept_empty, start_state, states);
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