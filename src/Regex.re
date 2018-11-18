open Nfa;

/** Convert a regex to an ε-free NFA using a slight modification of
    Glushkov's algorithm.

    (The modification: we label character sets rather than characters
     to prevent a state explosion.)
 */
module CharSet = {
  module S = Set.Make(Char);
  let to_string = (cs) =>
    switch (S.cardinal(cs)) {
      | 0 => "{}"
      | 1 => String.make(1, S.choose(cs))
      | 256 => "."
      | _ => "{" ++ String.concat(" ", List.map(String.make(1), S.elements(cs))) ++ "}"
    };
};

/** A 'letter' is a character set paired with an identifier that
    uniquely identifies the character set within the regex */
module Letter = {
  type t = (CharSet.S.t, state);
  let compare = ((_, x), (_, y)) => Pervasives.compare(x, y);
};

/** Sets of single letters */
module LetterSet = {
  module S = Set.Make(Letter);
  let (<+>) = S.union;
  let to_string = (ls) =>
    S.fold(
      ((c, i), str) => {
        str ++ CharSet.to_string(c) ++ "<sub>" ++ Int32.to_string(i) ++ "</sub> "
      },
      ls,
      ""
    );
};

/** Sets of letter pairs */
module Letter2Set = {
  module Pair = {
    type t = (Letter.t, Letter.t);
    let compare = (((_, w), (_, x)), ((_, y), (_, z))) =>
      Pervasives.compare((w, x), (y, z));
  };
  module S = Set.Make(Pair);
  let (<+>) = S.union;
  let (>>=) = (m, k) => LetterSet.S.fold((x, s) => k(x) <+> s, m, S.empty);
  let (<*>): (LetterSet.S.t, LetterSet.S.t) => S.t =
    (l, r) => l >>= (x => r >>= (y => S.singleton((x, y))));
  let to_string = (l2s) =>
    S.fold(
      (((c1, i1), (c2, i2)), str) => {
        str ++ CharSet.to_string(c1) ++ "<sub>" ++ Int32.to_string(i1) ++ "</sub>"
            ++ CharSet.to_string(c2) ++ "<sub>" ++ Int32.to_string(i2) ++ "</sub> "
      },
      l2s,
      "",
    );

};

type regex('c) =
  | Empty: regex('c) /* L = { } */
  | Eps: regex('c) /* L = {ε} */
  | Char('c): regex('c)
  | Alt(regex('c), regex('c)): regex('c)
  | Seq(regex('c), regex('c)): regex('c)
  | Star(regex('c)): regex('c);

type tree('a) =
  | One('a, tree('a))
  | Two('a, tree('a), tree('a))
  | Leaf('a);

/** Λ(r) is {ε} ∩ L(r); we represent it as a bool */

let rec l =
  fun
  | Empty => false
  | Eps => true
  | Char(_) => false
  | Alt(e, f) => l(e) || l(f)
  | Seq(e, f) => l(e) && l(f)
  | Star(_) => true;

/** firsts: P(r) = {c | ∃s.cs ∈ L(r) } */

let rec p =
  LetterSet.(
    fun
    | Empty
    | Eps => S.empty
    | Char(c) => S.singleton(c)
    | Alt(e, f) => p(e) <+> p(f)
    | Seq(e, f) =>
      p(e)
      <+> (
        if (l(e)) {
          p(f);
        } else {
          S.empty;
        }
      )
    | Star(e) => p(e)
  );

/** lasts: D(r) = {c | ∃s.sc ∈ L(r) } */

let rec d =
  LetterSet.(
    fun
    | Empty
    | Eps => S.empty
    | Char(c) => S.singleton(c)
    | Alt(f, e) => d(f) <+> d(e)
    | Seq(f, e) =>
      (
        if (l(e)) {
          d(f);
        } else {
          S.empty;
        }
      )
      <+> d(e)
    | Star(e) => d(e)
  );

/** factors of length 2: F(r) = {c₁c₂ | ∃s₁s₂.s₁c₁c₂s₂ ∈ L(R)} */

let rec f_ =
  Letter2Set.(
    fun
    | Empty
    | Eps
    | Char(_) => S.empty
    | Alt(e, f) => f_(e) <+> f_(f)
    | Seq(e, f) => f_(e) <+> f_(f) <+> (d(e) <*> p(f))
    | Star(e) => f_(e) <+> (d(e) <*> p(e))
  );

module StateMap = Map.Make(Int32);

module CharSetMap = Map.Make(CharSet.S);

let add_transition2 = (c, i, tm) => {
  let ss =
    switch (CharSetMap.find(c, tm)) {
    | exception Not_found => StateSet.S.empty
    | ss => ss
    };
  CharSetMap.add(c, StateSet.S.add(i, ss), tm);
};

let add_transition = (i1, c2, i2, sm) => {
  let tm =
    switch (StateMap.find(i1, sm)) {
    | exception Not_found => CharSetMap.empty
    | ss => ss
    };
  StateMap.add(i1, add_transition2(c2, i2, tm), sm);
};

let transition_map_of_factor_set = fs =>
  Letter2Set.S.fold(
    (((_, i1), (c2, i2)), sm) => add_transition(i1, c2, i2, sm),
    fs,
    StateMap.empty,
  );

let positions: LetterSet.S.t => StateSet.S.t =
  s => StateSet.S.of_list(List.map(snd, LetterSet.S.elements(s)));

let transition_map_of_letter_set: LetterSet.S.t => CharSetMap.t(StateSet.S.t) =
  s =>
    LetterSet.S.fold(
      ((c, i), tm) => {
        let entry =
          switch (CharSetMap.find(c, tm)) {
          | exception Not_found => StateSet.S.singleton(i)
          | s => StateSet.S.add(i, s)
          };
        CharSetMap.add(c, entry, tm);
      },
      s,
      CharSetMap.empty,
    );

type t = regex(CharSet.S.t);

let rec annotate: 'a. (int32, regex('a)) => (int32, regex(('a, int32))) =
  count =>
    fun
    | Empty => (count, Empty)
    | Eps => (count, Eps)
    | Char(c) => {
        let count = Int32.succ(count);
        let p = (c, count);
        (count, Char(p));
      }
    | Alt(e, f) => {
        let (count', e') = annotate(count, e);
        let (count'', f') = annotate(count', f);
        (count'', Alt(e', f'));
      }
    | Seq(e, f) => {
        let (count', e') = annotate(count, e);
        let (count'', f') = annotate(count', f);
        (count'', Seq(e', f'));
      }
    | Star(e) => {
        let (count', e') = annotate(count, e);
        (count', Star(e'));
      };

let rec string_of_annotated = (r) =>
  switch (r) {
  | Empty => ""
  | Eps => ""
  | Star(x) => string_of_annotated(x)
  | Seq(a, b) => string_of_annotated(a) ++ string_of_annotated(b)
  | Alt(a, b) => string_of_annotated(a) ++ string_of_annotated(b)
  | Char(x) =>
    switch (x) {
    | (c, i) => CharSet.to_string(c) ++ "<sub>" ++ Int32.to_string(i) ++ "</sub> "
    };
  };

let flatten_transitions: CharSetMap.t(StateSet.S.t) => CharMapStateSet.M.t(StateSet.S.t) =
  cm =>
    CharSetMap.fold(
      (cs, ss, cm) =>
        CharSet.S.fold(
          (c, cm) => {
            let entry =
              switch (CharMapStateSet.M.find(c, cm)) {
              | exception Not_found => StateSet.S.empty
              | ss => ss
              };
            CharMapStateSet.M.add(c, StateSet.S.union(ss, entry), cm);
          },
          cs,
          cm,
        ),
      cm,
      CharMapStateSet.M.empty,
    );

let compile = r => {
  /*** Give every character set in 'r' a unique identifier */
  let start_state = Int32.zero;
  let (_, annotated) = annotate(start_state, r);

  let nullable = l(annotated);
  let firsts = p(annotated);
  let lasts = d(annotated);
  let factors = f_(annotated);

  /*** The final states are the set of 'last' characters in r,
       (+ the start state if r accepts the empty string) */
  let finals =
    if (nullable) {
      StateSet.S.add(start_state, positions(lasts));
    } else {
      positions(lasts);
    };

  /*** Transitions arise from factor (pairs of character sets with a
       transition between them) ... */
  let transitions = transition_map_of_factor_set(factors);

  /*** ... and from the start state to the initial character sets. */
  let initial_transitions = transition_map_of_letter_set(firsts);
  let joint_transitions =
    StateMap.add(start_state, initial_transitions, transitions);

  /*** The 'next' function is built from the transition sets. */
  let next = s =>
    try (flatten_transitions(StateMap.find(s, joint_transitions))) {
    | Not_found => CharMapStateSet.M.empty
    };

  {start: start_state, finals, next,
  annotated: string_of_annotated(annotated),
  nullable,
  firsts: LetterSet.to_string(firsts),
  lasts: LetterSet.to_string(lasts),
  factors: Letter2Set.to_string(factors)};
};

/** Various basic and derived regex combinators */

let seq = (l, r) =>
  switch (l, r) {
  | (Eps, s)
  | (s, Eps) => s
  | (l, r) => Seq(l, r)
  };
let alt = (l, r) =>
  switch (l, r) {
  | (Char(c1), Char(c2)) => Char(CharSet.S.union(c1, c2))
  | (l, r) => Alt(l, r)
  };
let star = r => Star(r);
let plus = t => seq(t, star(t));
let eps = Eps;
let chr = c => Char(CharSet.S.singleton(c));
let opt = t => alt(t, eps);
let empty = Empty;

let range_ = (l, h) => {
  let rec loop = (i, h, acc) =>
    if (i == h) {
      CharSet.S.add(Char.chr(i), acc);
    } else {
      loop(succ(i), h, CharSet.S.add(Char.chr(i), acc));
    };
  loop(Char.code(l), Char.code(h), CharSet.S.empty);
};

let range = (l, h) => Char(range_(l, h));
let any = range(Char.chr(0), Char.chr(255));

exception Parse_error(string);

module Parse = {
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

  let regexp2parseTree = (parsed_regex) => {
    let rec unparse = r => switch(r) {
      | Empty => Leaf("Empty")
      | Eps => Leaf("Eps")
      | Star(s) => One("Star", unparse(s))
      | Seq(l, r) => Two("Seq", unparse(l), unparse(r))
      | Alt(l, r) => Two("Alt", unparse(l), unparse(r))
      | Char(s) => One("Char", Leaf(switch (CharSet.S.cardinal(s)) {
        | 0 => "{}"
        | 1 => String.make(1, CharSet.S.choose(s))
        | 256 => "."
        | _ => "{" ++ String.concat(" ", List.map(String.make(1), CharSet.S.elements(s))) ++ "}"
      }))
    };
    unparse(parsed_regex);
  };

};

let parse = Parse.parse;
let regexp2parseTree = Parse.regexp2parseTree;
