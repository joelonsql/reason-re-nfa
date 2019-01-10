open Regex;

exception Parse_error(string);

exception Fail;

type t =
  | Opt(t): t
  | Chr(char): t
  | Alt(t, t): t
  | Seq(t, t): t
  | Star(t): t
  | Plus(t): t
  | Bracketed(RegexBracket.t): t
  | Any: t
  | Eps: t;

let rec interpret: t => regex(_) =
  fun
  | Opt(t) => opt(interpret(t))
  | Chr(c) => chr(c)
  | Alt(l, r) => alt(interpret(l), interpret(r))
  | Seq(l, r) => seq(interpret(l), interpret(r))
  | Star(t) => star(interpret(t))
  | Plus(t) => plus(interpret(t))
  | Bracketed(elements) => Char(RegexBracket.interpret(elements))
  | Any => any
  | Eps => eps;

/* We've seen [.  Special characters:
   ^   (beginning of negation)  */
let re_parse_bracketed: list(char) => (t, list(char)) =
  fun
  | ['^', ...s] => {
      let (elements, rest) = RegexBracket.parse_elements(s);
      (Bracketed({negated: true, elements}), rest);
    }
  | [_, ..._] as s => {
      let (elements, rest) = RegexBracket.parse_elements(s);
      (Bracketed({negated: false, elements}), rest);
    }
  | [] => raise(Fail);

/** ratom ::= .
                <character>
                [ bracket ]
                ( ralt )           */

let rec re_parse_atom: list(char) => option((t, list(char))) =
  fun
  | ['(', ...rest] =>
    switch (re_parse_alt(rest)) {
    | (r, [')', ...rest]) => Some((r, rest))
    | _ => raise(Fail)
    }
  | ['[', ...rest] => Some(re_parse_bracketed(rest))
  | []
  | [')' | '|' | '*' | '?' | '+', ..._] => None
  | ['.', ...rest] => Some((Any, rest))
  | [h, ...rest] => Some((Chr(h), rest))

/** rsuffixed ::= ratom
                    atom *
                    atom +
                    atom ?         */

and re_parse_suffixed: list(char) => option((t, list(char))) =
  s =>
    switch (re_parse_atom(s)) {
    | None => None
    | Some((r, ['*', ...rest])) => Some((Star(r), rest))
    | Some((r, ['+', ...rest])) => Some((Plus(r), rest))
    | Some((r, ['?', ...rest])) => Some((Opt(r), rest))
    | Some((r, rest)) => Some((r, rest))
    }

/** rseq ::= <empty>
               rsuffixed rseq      */

and re_parse_seq: list(char) => (t, list(char)) =
  (s: list(char)) =>
    switch (re_parse_suffixed(s)) {
    | None => (Eps, s)
    | Some((r, rest)) =>
      let (r', s') = re_parse_seq(rest);
      (Seq(r, r'), s');
    }

/** ralt ::= rseq
               rseq | ralt         */

and re_parse_alt = (s: list(char)) =>
  switch (re_parse_seq(s)) {
  | (r, ['|', ...rest]) =>
    let (r', s') = re_parse_alt(rest);
    (Alt(r, r'), s');
  | (r, rest) => (r, rest)
  };

let parse = s =>
  interpret(
    switch (re_parse_alt(Common.explode(s))) {
    | (r, []) => r
    | exception Fail => raise(Parse_error(s))
    | (_, [_, ..._]) => raise(Parse_error(s))
    },
  );

let unparse_charset = s => {
  let pos = RegexBracket.unparse(~complement=false, s)
  and neg = RegexBracket.unparse(~complement=true, CharSet.diff(any_, s));
  if (String.length(pos) <= String.length(neg)) {
    pos;
  } else {
    neg;
  };
};