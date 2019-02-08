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

let merge_linear = (~max_length=8, ~max_cardinality=32, input_dfa) => {
  let rec merge_linear = (src, ranges, cardinality, dst, dfa, seen, lengths) =>
    if (List.length(RangeSetListSet.choose(ranges)) > 0
        && exists_transition((src, ranges, dst), seen)) {
      (dfa, seen);
    } else {
      let seen = add_transition((src, ranges, dst), seen);
      let (src, ranges, cardinality, dfa) =
        if (!StateSetSet.mem(dst, input_dfa.finals)
            && List.length(RangeSetListSet.choose(ranges))
            < (
                switch (StateSetMap.find(src, lengths)) {
                | exception Not_found => max_length
                | length => min(length, max_length)
                }
              )
            && (
              count_children(dst, input_dfa) == 1
              && count_parents(dst, input_dfa) == 1
              || cardinality
              * count_children(dst, input_dfa) <= max_cardinality
            )) {
          (src, ranges, cardinality * count_children(dst, input_dfa), dfa);
        } else {
          let dfa = add_transition((src, ranges, dst), dfa);
          (dst, RangeSetListSet.empty, count_children(dst, input_dfa), dfa);
        };
      RangeSetListSetMap.fold(
        (ranges', dst', (dfa, seen)) =>
          merge_linear(
            src,
            RangeSetListSet.merge(ranges, ranges'),
            cardinality,
            dst',
            dfa,
            seen,
            lengths,
          ),
        try (StateSetMap.find(dst, input_dfa.transitions)) {
        | Not_found => RangeSetListSetMap.empty
        },
        (dfa, seen),
      );
    };

  let length_dfa = {
    let (dfa, _) =
      RangeSetListSetMap.fold(
        (ranges, dst, (dfa, seen)) =>
          merge_linear(
            input_dfa.start,
            ranges,
            count_children(input_dfa.start, input_dfa),
            dst,
            dfa,
            seen,
            StateSetMap.empty,
          ),
        StateSetMap.find(input_dfa.start, input_dfa.transitions),
        (singleton(input_dfa.start), singleton(input_dfa.start)),
      );
    set_finals(input_dfa.finals, dfa);
  };

  let length_map =
    StateSetMap.fold(
      (src, ranges, length_map) =>
        RangeSetListSetMap.fold(
          (ranges, _, length_map) => {
            let length = List.length(RangeSetListSet.choose(ranges));
            let cur_length =
              try (StateSetMap.find(src, length_map)) {
              | Not_found => length
              };
            StateSetMap.add(src, min(length, cur_length), length_map);
          },
          ranges,
          length_map,
        ),
      length_dfa.transitions,
      StateSetMap.empty,
    );

  let (dfa, _) =
    RangeSetListSetMap.fold(
      (ranges, dst, (dfa, seen)) =>
        merge_linear(
          input_dfa.start,
          ranges,
          count_children(input_dfa.start, input_dfa),
          dst,
          dfa,
          seen,
          length_map,
        ),
      StateSetMap.find(input_dfa.start, input_dfa.transitions),
      (singleton(input_dfa.start), singleton(input_dfa.start)),
    );
  set_finals(input_dfa.finals, dfa);
};

let merge_branches: t => t =
  input_dfa => {
    let group_by: transitions => transitions =
      transitions => {
        let fold_ranges_map:
          RangeSetListSetMap.t(state) => StateSetMap.t(RangeSetListSet.t) =
          ranges_map =>
            RangeSetListSetMap.fold(
              (ranges, dst, dst_map) => {
                let ranges' =
                  switch (StateSetMap.find(dst, dst_map)) {
                  | exception Not_found => ranges
                  | ranges' => RangeSetListSet.union(ranges', ranges)
                  };
                let dst_map = StateSetMap.add(dst, ranges', dst_map);
                dst_map;
              },
              ranges_map,
              StateSetMap.empty,
            );

        StateSetMap.fold(
          (src, ranges_map, acc) =>
            StateSetMap.fold(
              (dst, ranges, acc) =>
                StateSetMap.add(
                  src,
                  switch (StateSetMap.find(src, acc)) {
                  | exception Not_found =>
                    RangeSetListSetMap.singleton(ranges, dst)
                  | ranges_map =>
                    RangeSetListSetMap.add(ranges, dst, ranges_map)
                  },
                  acc,
                ),
              fold_ranges_map(ranges_map),
              acc,
            ),
          transitions,
          StateSetMap.empty,
        );
      };

    StateSetMap.fold(
      (src, ranges_map, dfa) =>
        RangeSetListSetMap.fold(
          (ranges, dst, dfa) => add_transition((src, ranges, dst), dfa),
          ranges_map,
          dfa,
        ),
      group_by(input_dfa.transitions),
      singleton(input_dfa.start),
    )
    |> set_finals(input_dfa.finals);
  };

let merge_ranges: t => t =
  input_dfa => {
    let group_by: transitions => transitions =
      transitions => {
        let fold_ranges_map:
          RangeSetListSetMap.t(state) => StateSetMap.t(RangeSetListSet.t) =
          ranges_map =>
            RangeSetListSetMap.fold(
              (ranges, dst, dst_map) =>
                StateSetMap.add(
                  dst,
                  switch (StateSetMap.find(dst, dst_map)) {
                  | exception Not_found => ranges
                  | ranges' =>
                    RangeSetListSet.singleton([
                      RangeSet.union(
                        RangeSetListSet.choose_strict(ranges),
                        RangeSetListSet.choose_strict(ranges'),
                      ),
                    ])
                  },
                  dst_map,
                ),
              ranges_map,
              StateSetMap.empty,
            );

        StateSetMap.fold(
          (src, ranges_map, acc) =>
            StateSetMap.fold(
              (dst, ranges, acc) =>
                StateSetMap.add(
                  src,
                  switch (StateSetMap.find(src, acc)) {
                  | exception Not_found =>
                    RangeSetListSetMap.singleton(ranges, dst)
                  | ranges_map =>
                    RangeSetListSetMap.add(ranges, dst, ranges_map)
                  },
                  acc,
                ),
              fold_ranges_map(ranges_map),
              acc,
            ),
          transitions,
          StateSetMap.empty,
        );
      };

    StateSetMap.fold(
      (src, ranges_map, dfa) =>
        RangeSetListSetMap.fold(
          (ranges, dst, dfa) => add_transition((src, ranges, dst), dfa),
          ranges_map,
          dfa,
        ),
      group_by(input_dfa.transitions),
      singleton(input_dfa.start),
    )
    |> set_finals(input_dfa.finals);
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

let ll_match_dfa = (accept_empty, start_state, states) => {j|
define zeroext i1 @match_dfa(i8*) {
  %s = alloca i8*, align 8
  %match = alloca i8, align 1
  store i8* %0, i8** %s, align 8
  store i8 $accept_empty, i8* %match, align 1
  br label %$start_state

$states

detect_end_of_string:
  %s_ptr = load i8*, i8** %s, align 8
  %chr = load i8, i8* %s_ptr, align 1
  %chr_is_zero = icmp eq i8 %chr, 0
  br i1 %chr_is_zero, label %done, label %miss

miss:
  store i8 0, i8* %match, align 1
  br label %done

done:
  %match_val = load i8, i8* %match, align 1
  %ret = trunc i8 %match_val to i1
  ret i1 %ret
}

define i32 @main(i32, i8** nocapture readonly) {
  %argv = getelementptr inbounds i8*, i8** %1, i64 1
  %input_string = load i8*, i8** %argv, align 8
  %matched = tail call zeroext i1 @match_dfa(i8* %input_string)
  %ret = zext i1 %matched to i32
  ret i32 %ret
}
|j};

let js_match_dfa = (accept_empty, start_state, states) => {j|
  let i = 0;
  let state = $start_state;
  let match = $accept_empty;
  let length = s.length;
  while (true) {
    loop_switch:
    switch (state) {
      case -1:
        if (i == length) {
          /* end of string reached */
          return match;
        } else {
          /* string did not match */
          return false;
        }
$states
    }
  }
|j};

let ll_states = (src_state, switch_state, goto_dsts) => {j|
$src_state:
  %$src_state.s_ptr = load i8*, i8** %s, align 8
  $switch_state
  $goto_dsts
|j};

let js_states = (src_state, switch_state) => {j|
case $src_state:
$switch_state
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

let ll_switch = (str_len, cases, itype, src_state) =>
  switch (str_len) {
  | n when n >= 1 && n <= 8 => {j|
  switch $itype %$src_state.chr, label %detect_end_of_string [
    $cases
  ]
    |j}
  | n when n > 8 => {j|
$cases
br label %detect_end_of_string
    |j}
  | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
  };

let js_switch = (str_len, cases) => {
  let exp =
    switch (str_len) {
    | 1 => "s.charCodeAt(i)"
    | n when n > 1 => {j|s.substring(i, i + $str_len)|j}
    | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
    };
  {j|
        switch ($exp) {
          $cases
          default:
            state = -1;
            break loop_switch;
        }
  |j};
};

let js_labeled_block = (src_state, code) => {
  {j|
        state$src_state:
        while (true) {
        $code
        }
  |j};
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

let js_switch_case_value = (ival, string_escaped) =>
  if (String.length(string_escaped) > 0) {
    {j|
          case $ival: /* $string_escaped */
    |j};
  } else {
    {j|
          case $ival:
    |j};
  };

let js_switch_case_code =
    (str_len, src_state, dst_state, set_match, inline_or_branch) => {j|
            /* $src_state -> $dst_state */
            $set_match
            i += $str_len;
            $inline_or_branch
    |j};

let js_switch_case =
    (
      str_len,
      ival,
      src_state,
      dst_state,
      string_escaped,
      in_accepting_state,
      inline_or_branch,
    ) => {j|
          case $ival:
            /* $src_state -($string_escaped)-> $dst_state */
            match = $in_accepting_state;
            i += $str_len;
            $inline_or_branch
    |j};

let js_switch_case_state = (src_state, code) => {j|
      case $src_state: $code
|j};

let js_goto_irreducible_state = dst_state => {j|
            state = $dst_state;
            break loop_switch;
|j};

let js_continue = dst_state => {j|
            continue state$dst_state;
|j};

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
        (
          {j|
              state = -1;
              break loop_switch;
              |j},
          todo,
        );
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
                    [js_switch_case_value(ival, string_escaped), ...acc];
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
                | (true, _, _) => (js_continue(dst_state), todo)

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
                    js_goto_irreducible_state(dst_state),
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
                  ++ js_switch_case_code(
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
            js_labeled_block(
              src_state,
              js_switch(str_len, String.concat("", List.rev(cases))),
            );
          } else {
            js_switch(str_len, String.concat("", List.rev(cases)));
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
                  js_switch_case_state(src_state, code),
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

  js_match_dfa(accept_empty, start_state, states);
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