exception Bug(string);

module JavaScript = {
  let match_dfa = (accept_empty, start_state, states) => {j|
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

  let switch_case_state = (src_state, code) => {j|
    case $src_state: $code
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

  let make_switch = (_, str_len, cases) => {
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

  let switch_case_value = (ival, string_escaped) =>
    if (String.length(string_escaped) > 0) {
      {j|
        case $ival: /* $string_escaped */
      |j};
    } else {
      {j|
        case $ival:
      |j};
    };
  let switch_case_code =
      (str_len, src_state, dst_state, set_match, inline_or_branch) => {j|
        /* $src_state -> $dst_state */
        $set_match
        i += $str_len;
        $inline_or_branch
    |j};
  let continue = dst_state => {j|
    continue state$dst_state;
  |j};
  let goto_irreducible_state = dst_state => {j|
    state = $dst_state;
    break loop_switch;
  |j};
};

module LLVMIR = {
  let match_dfa = (accept_empty, start_state, states) => {j|
define zeroext i1 @match_dfa(i8*) {
  %s = alloca i8*, align 8
  %match = alloca i8, align 1
  store i8* %0, i8** %s, align 8
  store i8 $accept_empty, i8* %match, align 1
  br label %state$start_state

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

  let switch_case_state = (src_state, code) => {j|
state$src_state:
$code
|j};

  let labeled_block = (src_state, code) => {j|
state$src_state:
$code
|j};

  let break_loop_switch = {j|
br label %detect_end_of_string
|j};

  let make_itype = str_len =>
    if (str_len > 8) {
      "<" ++ string_of_int(str_len) ++ " x i8>";
    } else {
      "i" ++ string_of_int(str_len * 8);
    };

  let make_switch = (src_state, str_len, cases) => {
    let src_state = "state" ++ src_state;
    let itype = make_itype(str_len);
    (
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
      }
    )
    ++ (
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
      }
    );
  };

  let switch_case_value = (ival, string_escaped) =>
    if (String.length(string_escaped) > 0) {
      {j|
            case $ival: /* $string_escaped */
      |j};
    } else {
      {j|
            case $ival:
      |j};
    };
  let switch_case_code =
      (str_len, src_state, dst_state, set_match, inline_or_branch) => {j|
            /* $src_state -> $dst_state */
            $set_match
            i += $str_len;
            $inline_or_branch
    |j};
  let continue = dst_state => {j|
              continue state$dst_state;
  |j};
  let goto_irreducible_state = dst_state => {j|
              state = $dst_state;
              break loop_switch;
  |j};
};