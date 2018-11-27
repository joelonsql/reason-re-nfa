open Regex;

exception Parse_error(string);

exception Fail;

/** ratom ::= .
              <character>
              ( ralt )           */

let rec re_parse_atom: list(char) => option((regex(_), list(char))) =
  fun
  | ['(', ...rest] =>
    switch (re_parse_alt(rest)) {
    | (r, [')', ...rest]) => Some((r, rest))
    | _ => raise(Fail)
    }
  | []
  | [')' | '|' | '*' | '?' | '+', ..._] => None
  | ['.', ...rest] => Some((any, rest))
  | [h, ...rest] => Some((chr(h), rest))
/** rsuffixed ::= ratom
                  atom *
                  atom +
                  atom ?         */

and re_parse_suffixed: list(char) => option((regex(_), list(char))) = (char_list) =>
    switch (re_parse_atom(char_list)) {
    | None => None
    | Some((r, ['*', ...rest])) => Some((star(r), rest))
    | Some((r, ['+', ...rest])) => Some((plus(r), rest))
    | Some((r, ['?', ...rest])) => Some((opt(r), rest))
    | Some((r, rest)) => Some((r, rest))
    }
/** rseq ::= <empty>
              rsuffixed rseq      */

and re_parse_seq = (char_list: list(char)) =>
  switch (re_parse_suffixed(char_list)) {
  | None => (eps, char_list)
  | Some((r, rest)) =>
    let (r', char_list') = re_parse_seq(rest);
    (seq(r, r'), char_list');
  }
/** ralt ::= rseq
              rseq | ralt         */

and re_parse_alt = (char_list: list(char)) =>
  switch (re_parse_seq(char_list)) {
  | (r, ['|', ...rest]) =>
    let (r', char_list') = re_parse_alt(rest);
    (alt(r, r'), char_list');
  | (r, rest) => (r, rest)
  };

let explode = (regex: string) => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [regex.[i], ...l]);
    };
  exp(String.length(regex) - 1, []);
};

let parse = (regex: string) =>
  switch (re_parse_alt(explode(regex))) {
  | (r, []) => r
  | exception Fail => raise(Parse_error(regex))
  | (_, [_, ..._]) => raise(Parse_error(regex))
  };
