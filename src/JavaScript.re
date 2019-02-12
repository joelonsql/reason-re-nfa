exception Bug(string);

let match_dfa = (accept_empty, start_state, states) => {
  let accept_empty = accept_empty ? "true" : "false";
  {j|
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
};

let switch_case_state = (src_state, code) => {j|
    case $src_state:
    state$src_state:
      while (true) {
        $code
      }
  |j};

let labeled_block = (src_state, code) => {j|
    state$src_state:
      while (true) {
        $code
      }
  |j};

let break_loop_switch = {j|
    state = -1;
    break loop_switch;
  |j};

let make_switch = (_, str_len, cases_and_code) => {
  let cases =
    String.concat(
      "",
      List.map(((case, code)) => case ++ code, cases_and_code),
    );

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
            $break_loop_switch
        }
  |j};
};

let switch_case_value = (_, str_len, string, _, _) => {
  let (ival, string_escaped) =
    str_len == 1 ?
      (
        string_of_int(Char.code(string.[0])),
        Common.escape_string(string),
      ) :
      ({j|"$string"|j}, "");

  if (String.length(string_escaped) > 0) {
    {j|
        case $ival: /* $string_escaped */
      |j};
  } else {
    {j|
        case $ival:
      |j};
  };
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
    | _ => in_accepting_state ? "match = true;" : "match = false;"
    };
  {j|
        /* $src_state -> $dst_state */
        $set_match
        i += $str_len;
        $inline_or_branch
    |j};
};

let continue = dst_state => {j|
    continue state$dst_state;
  |j};

let goto_irreducible_state = dst_state => {j|
    state = $dst_state;
    break loop_switch;
  |j};