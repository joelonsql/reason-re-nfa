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

and re_parse_suffixed: list(char) => option((regex(_), list(char))) =
  s =>
    switch (re_parse_atom(s)) {
    | None => None
    | Some((r, ['*', ...rest])) => Some((star(r), rest))
    | Some((r, ['+', ...rest])) => Some((plus(r), rest))
    | Some((r, ['?', ...rest])) => Some((opt(r), rest))
    | Some((r, rest)) => Some((r, rest))
    }
/** rseq ::= <empty>
              rsuffixed rseq      */

and re_parse_seq = (s: list(char)) =>
  switch (re_parse_suffixed(s)) {
  | None => (eps, s)
  | Some((r, rest)) =>
    let (r', s') = re_parse_seq(rest);
    (seq(r, r'), s');
  }
/** ralt ::= rseq
              rseq | ralt         */

and re_parse_alt = (s: list(char)) =>
  switch (re_parse_seq(s)) {
  | (r, ['|', ...rest]) =>
    let (r', s') = re_parse_alt(rest);
    (alt(r, r'), s');
  | (r, rest) => (r, rest)
  };

let explode = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [s.[i], ...l]);
    };
  exp(String.length(s) - 1, []);
};

let parse = s =>
  switch (re_parse_alt(explode(s))) {
  | (r, []) => r
  | exception Fail => raise(Parse_error(s))
  | (_, [_, ..._]) => raise(Parse_error(s))
  };
