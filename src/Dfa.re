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

let group_by_str_len:
  transitions => StateSetMap.t(StrLenMap.t(StringMap.t(state))) =
  transitions =>
    StateSetMap.fold(
      (src, string_map, state_set_map) => {
        let str_len_map =
          StringMap.fold(
            (string, dst, str_len_map) => {
              let str_len = Int32.of_int(String.length(string));
              let string_map' =
                try (StrLenMap.find(str_len, str_len_map)) {
                | Not_found => StringMap.empty
                };
              let string_map' = StringMap.add(string, dst, string_map');
              StrLenMap.add(str_len, string_map', str_len_map);
            },
            string_map,
            StrLenMap.empty,
          );
        StateSetMap.add(src, str_len_map, state_set_map);
      },
      transitions,
      StateSetMap.empty,
    );

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

let ll_states = (src_state, switch_state, goto_dsts) => {j|
$src_state:
  %$src_state.s_ptr = load i8*, i8** %s, align 8
  $switch_state
   br label %detect_end_of_string
   $goto_dsts
|j};

let ll_load = (str_len, itype, src_state) =>
  switch (str_len) {
  | 1 => {j|
  %$src_state.$str_len.chr = load i8, i8* %$src_state.s_ptr, align 1
  %$src_state.$str_len.next_ptr = getelementptr inbounds i8, i8* %$src_state.s_ptr, i32 $str_len
    |j}
  | n when n > 1 && n <= 8 => {j|
  %$src_state.$str_len.chr_ptr = bitcast i8* %$src_state.s_ptr to $itype*
  %$src_state.$str_len.chr = load $itype, $itype* %$src_state.$str_len.chr_ptr, align 1
  %$src_state.$str_len.next_ptr = getelementptr inbounds i8, i8* %$src_state.s_ptr, i32 $str_len
    |j}
  | n when n > 8 => {j|
  %$src_state.$str_len.vptr = bitcast i8* %$src_state.s_ptr to $itype*
  %$src_state.$str_len.rhs = load $itype, $itype* %$src_state.$str_len.vptr, align 1
  %$src_state.$str_len.next_ptr = getelementptr inbounds i8, i8* %$src_state.s_ptr, i32 $str_len
    |j}
  | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
  };

let ll_switch = (str_len, cases, itype, src_state) =>
  switch (str_len) {
  | n when n >= 1 && n <= 8 => {j|
  switch $itype %$src_state.$str_len.chr, label %$src_state.$str_len.default [
    $cases
  ]
$src_state.$str_len.default:
    |j}
  | n when n > 8 => {j|
$cases
    |j}
  | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
  };

let ll_switch_case =
    (i, str_len, itype, ival, src_state, dst_state, string_escaped, bits) =>
  switch (str_len) {
  | n when n >= 1 && n <= 8 => {j|
    $itype $ival, label %$src_state.$str_len.goto.$dst_state ; $string_escaped
    |j}
  | n when n > 8 => {j|
  %$src_state.$str_len.$i.cmp_mask = icmp eq $itype %$src_state.$str_len.rhs, $ival ; $string_escaped
  %$src_state.$str_len.$i.cmp_int = bitcast <$str_len x i1> %$src_state.$str_len.$i.cmp_mask to i$str_len
  %$src_state.$str_len.$i.is_equal = icmp eq i$str_len %$src_state.$str_len.$i.cmp_int, -1 ; 0b$bits
  br i1 %$src_state.$str_len.$i.is_equal, label %$src_state.$str_len.goto.$dst_state, label %$src_state.$str_len.$i.try_next
$src_state.$str_len.$i.try_next:
    |j}
  | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
  };

let ll_goto_state = (str_len, src_state, dst_state, in_accepting_state) => {j|
$src_state.$str_len.goto.$dst_state:
  store i8* %$src_state.$str_len.next_ptr, i8** %s, align 8
  store i8 $in_accepting_state, i8* %match, align 1
  br label %$dst_state
    |j};

let ll_state_goto_handlers = goto_dsts => {
  String.concat(
    "",
    List.map(((_, llvmir)) => llvmir, StateSetMap.bindings(goto_dsts)),
  );
};

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
                (str_len, string_map, (switch_states, list_goto_dsts)) => {
                  let str_len = Int32.to_int(str_len);
                  let itype =
                    if (str_len > 8) {
                      "<" ++ string_of_int(str_len) ++ " x i8>";
                    } else {
                      "i" ++ string_of_int(str_len * 8);
                    };
                  let (_, cases, goto_dsts) =
                    StringMap.fold(
                      (string, dst, (i, acc, goto_dsts)) => {
                        assert(String.length(string) == str_len);
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
                                str_len,
                                src_state,
                                dst_state,
                                in_accepting_state,
                              ),
                              goto_dsts,
                            );
                          },
                        );
                      },
                      string_map,
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
                    [ll_state_goto_handlers(goto_dsts), ...list_goto_dsts],
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