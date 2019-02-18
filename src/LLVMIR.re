exception Bug(string);

let match_dfa = (accept_empty, start_state, states) => {
  let accept_empty = accept_empty ? "1" : "0";
  {j|
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
};

let switch_case_state = states_and_code =>
  String.concat(
    "",
    List.map(
      ((state, code)) => {
        let src_state = string_of_int(state);
        {j|
  state$src_state:
$code
  |j};
      },
      states_and_code,
    ),
  );

let labeled_block = (src_state, code) => {j|
  br label state$src_state
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

let make_switch = (src_state, str_len, cases_and_code) => {
  let cases =
    String.concat("", List.map(((case, _)) => case, cases_and_code));
  let code =
    String.concat("", List.map(((_, code)) => code, cases_and_code));
  let src_state = "state" ++ src_state;
  let itype = make_itype(str_len);
  {j|
  %$src_state.s_ptr = load i8*, i8** %s, align 8
|j}
  ++ (
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
$code
    |j}
    | n when n > 8 => {j|
$cases
  br label %detect_end_of_string
$code
    |j}
    | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
    }
  );
};

let encode_string_as_int_or_vector = s => {
  let rec exp = (pos, i) =>
    if (pos < 0) {
      i;
    } else {
      exp(
        pos - 1,
        Int64.add(
          i,
          Int64.shift_left(Int64.of_int(Char.code(s.[pos])), 8 * pos),
        ),
      );
    };
  let length = String.length(s);
  if (length <= 8) {
    Int64.to_string(exp(String.length(s) - 1, Int64.zero));
  } else {
    "<"
    ++ String.concat(
         ",",
         List.map(
           chr => " i8 " ++ string_of_int(Char.code(chr)),
           Common.explode(s),
         ),
       )
    ++ " >";
  };
};

let switch_case_value = (i, str_len, string, src_state, dst_state) => {
  let ival = encode_string_as_int_or_vector(string);
  let string_escaped = Common.escape_string(string);
  let itype = make_itype(str_len);
  let bits = String.make(str_len, '1');
  let src_state = "state" ++ src_state;
  switch (str_len) {
  | n when n >= 1 && n <= 8 => {j|
    $itype $ival, label %$src_state.goto.$dst_state ; $string_escaped
    |j}
  | n when n > 8 => {j|
  %$src_state.goto.$dst_state.$i.cmp_mask = icmp eq $itype %$src_state.rhs, $ival ; $string_escaped
  %$src_state.goto.$dst_state.$i.cmp_int = bitcast <$str_len x i1> %$src_state.goto.$dst_state.$i.cmp_mask to i$str_len
  %$src_state.goto.$dst_state.$i.is_equal = icmp eq i$str_len %$src_state.goto.$dst_state.$i.cmp_int, -1 ; 0b$bits
  br i1 %$src_state.goto.$dst_state.$i.is_equal, label %$src_state.goto.$dst_state, label %$src_state.goto.$dst_state.$i.try_next
  $src_state.goto.$dst_state.$i.try_next:
    |j}
  | _ => raise(Bug("Unexpected str_len: " ++ string_of_int(str_len)))
  };
};

let switch_case_code =
    (
      _,
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
        "store i8 1, i8* %match, align 1" : "store i8 0, i8* %match, align 1"
    };
  {j|
  state$src_state.goto.$dst_state:
  store i8* %state$src_state.next_ptr, i8** %s, align 8
  $set_match
  $inline_or_branch
    |j};
};

let continue = dst_state => {j|
  br label %state$dst_state
  |j};

let goto_irreducible_state = dst_state => {j|
  br label %state$dst_state
  |j};