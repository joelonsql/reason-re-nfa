exception Bug(string);

let match_dfa = (accept_empty, start_state, states) => {
  let accept_empty = accept_empty ? "1" : "0";
  {j|
(module
(type \$match_dfa (func (param i32) (result i32)))
(import "js" "mem" (memory \$js.mem 0))
(func \$match_dfa (export "match_dfa") (type \$match_dfa) (param \$s i32) (result i32)
(local \$i i32)
(local \$state i32)
(local \$match i32)
(set_local \$i (get_local \$s))
(set_local \$state (i32.const $start_state))
(set_local \$match (i32.const $accept_empty))
(loop \$loop_switch
(block \$detect_end_of_string
$states
)
(block \$miss
(br_if \$miss
(i32.load8_u (get_local \$i))
)
(return (get_local \$match))
)
)
(return (i32.const 0))
)
)
|j};
};

let switch_case_state = states_and_code => {
  let build_br_table = states => {
    let max =
      List.hd(List.rev(List.sort((a, b) => compare(a, b), states)));
    let rec aux = acc =>
      fun
      | (-1) => acc
      | state =>
        aux(
          [
            List.mem(state, states) ?
              "$state" ++ string_of_int(state) : "$invalid_state",
            ...acc,
          ],
          pred(state),
        );
    let br_table = String.concat(" ", aux([], max));
    {j|
(get_local \$state)
(br_table $br_table \$invalid_state)
|j};
  };

  let rec build_branch = states =>
    fun
    | [] => build_br_table(states)
    | [(state, code), ...rest] => {
        let innerBlocks = build_branch([state, ...states], rest);
        {j|
(block \$state$state
$innerBlocks
)
$code
|j};
      };

  let code = build_branch([], states_and_code);
  {j|
(block \$invalid_state
$code
)
(i32.const -1)
(return)
|j};
};

let labeled_block = (src_state, code) => {j|
(loop \$state$src_state
$code
)
  |j};

let break_loop_switch = {j|
(br \$detect_end_of_string)
|j};

let make_switch = (src_state, str_len, cases_and_code) => {
  let cases =
    String.concat(
      "",
      List.map(
        ((case, code)) =>
          {j|
(block \$miss
(block \$match
$case
(br \$miss)
)
$code
)
|j},
        cases_and_code,
      ),
    );

  {j|
$cases
(br \$detect_end_of_string)
|j};
};

let switch_case_value = (i, str_len, string, src_state, dst_state) => {
  let ival = LLVMIR.encode_string_as_int_or_vector(string);
  let string_escaped = Common.escape_string(string);
  let load_op =
    switch (str_len) {
    | 1 => "i32.load8_u"
    | 2 => "i32.load16_u"
    | 4 => "i32.load32_u"
    | _ => "???"
    };
  {j|
(i32.const $ival) ;; $string_escaped
($load_op (get_local \$i))
(i32.eq)
(br_if \$match)
|j};
};

let switch_case_code =
    (
      str_len,
      src_state,
      dst_state,
      cur_in_accepting_state,
      in_accepting_state,
      inline_or_branch,
    ) => {
  let set_match =
    switch (cur_in_accepting_state) {
    | Some(m) when m == in_accepting_state => "" /* match variable already has correct value from previous state */
    | _ =>
      in_accepting_state ?
        "(set_local $match (i32.const 1))" : "(set_local $match (i32.const 0))"
    };
  {j|
;; $src_state -> $dst_state
$set_match
(set_local \$i
(i32.add
(get_local \$i)
(i32.const $str_len)
)
)
$inline_or_branch
|j};
};

let continue = dst_state => {j|
(br \$state$dst_state)
|j};

let goto_irreducible_state = dst_state => {j|
(set_local \$state
(i32.const $dst_state)
)
(br \$loop_switch)
|j};