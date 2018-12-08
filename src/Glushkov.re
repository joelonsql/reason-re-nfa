/** Convert a regex to an ε-free NFA using a slight modification of
    Glushkov's algorithm.

    (The modification: we label character sets rather than characters
     to prevent a state explosion.)
 */

open Regex;

type t = {
  nfa: Nfa.nfa,
  annotated: string,
  nullable: bool,
  firsts: string,
  lasts: string,
  factors: string,
  transitions: array(array(string))
};

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
    | Eps => LetterSet.empty
    | Char(c) => singleton(c)
    | Alt(e, f) => p(e) <+> p(f)
    | Seq(e, f) => p(e) <+> (
        if (l(e)) {
          p(f);
        } else {
          LetterSet.empty;
        }
      )
    | Star(e) => p(e)
  );

/** lasts: D(r) = {c | ∃s.sc ∈ L(r) } */

let rec d =
  LetterSet.(
    fun
    | Empty
    | Eps => LetterSet.empty
    | Char(c) => singleton(c)
    | Alt(f, e) => d(f) <+> d(e)
    | Seq(f, e) =>
      (
        if (l(e)) {
          d(f);
        } else {
          LetterSet.empty;
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
    | Char(_) => Letter2Set.empty
    | Alt(e, f) => f_(e) <+> f_(f)
    | Seq(e, f) => f_(e) <+> f_(f) <+> (d(e) <*> p(f))
    | Star(e) => f_(e) <+> (d(e) <*> p(e))
  );

let transition_map_of_factor_set = (factors) => {
  let add_transition = (from_state, char_set, to_state, state_map) => {
    let char_set_map =
      switch (StateMapCharSetMapStateSet.find(from_state, state_map)) {
      | exception Not_found => CharSetMapStateSet.empty
      | char_set_map => char_set_map
      };
    let state_set =
      switch (CharSetMapStateSet.find(char_set, char_set_map)) {
      | exception Not_found => StateSet.empty
      | state_set => state_set
      };
    let state_set = StateSet.add(to_state, state_set);
    let char_set_map = CharSetMapStateSet.add(char_set, state_set, char_set_map);
    StateMapCharSetMapStateSet.add(from_state, char_set_map, state_map);
  };
  Letter2Set.fold(
    (((_, from_state), (char_set, to_state)), state_map) => add_transition(from_state, char_set, to_state, state_map),
    factors,
    StateMapCharSetMapStateSet.empty
  );
};

let positions: LetterSet.t => StateSet.t = (letter_set) =>
  StateSet.of_list(List.map(snd, LetterSet.elements(letter_set)));

let transition_map_of_letter_set: LetterSet.t => CharSetMapStateSet.t(StateSet.t) = (letter_set) =>
  LetterSet.fold(
    ((char_set, state), char_set_map) => {
      let entry =
        switch (CharSetMapStateSet.find(char_set, char_set_map)) {
        | exception Not_found => StateSet.singleton(state)
        | state_set => StateSet.add(state, state_set)
        };
      CharSetMapStateSet.add(char_set, entry, char_set_map);
    },
    letter_set,
    CharSetMapStateSet.empty
  );

let flatten_transitions: CharSetMapStateSet.t(StateSet.t) => CharMapStateSet.t(StateSet.t) = (char_map) =>
  CharSetMapStateSet.fold(
    (char_set, state_set, char_map) =>
    CharSet.fold(
      (char, char_map) => {
        let entry =
          switch (CharMapStateSet.find(char, char_map)) {
          | exception Not_found => StateSet.empty
          | state_set => state_set
          };
        CharMapStateSet.add(char, StateSet.union(state_set, entry), char_map);
      },
      char_set,
      char_map
    ),
    char_map,
    CharMapStateSet.empty,
  );

let compile: regex('c) => t = (r) => {
  /*** Give every character set in 'r' a unique identifier */
  let start: Nfa.state = Int32.zero;
  let annotated = AnnotatedRegex.annotate(r);

  let nullable = l(annotated);
  let firsts = p(annotated);
  let lasts = d(annotated);
  let factors = f_(annotated);

  /*** The final states are the set of 'last' characters in r,
       (+ the start state if r accepts the empty string) */
  let finals =
    if (nullable) {
      StateSet.add(start, positions(lasts));
    } else {
      positions(lasts);
    };

  /*** Transitions arise from factor (pairs of character sets with a
       transition between them) ... */
  let factor_transitions = transition_map_of_factor_set(factors);

  /*** ... and from the start state to the initial character sets. */
  let initial_transitions = transition_map_of_letter_set(firsts);
  let joint_transitions =
    StateMapCharSetMapStateSet.add(start, initial_transitions, factor_transitions);

  /*** The 'next' function is built from the transition sets. */
  let next = (state) =>
    try (flatten_transitions(StateMapCharSetMapStateSet.find(state, joint_transitions))) {
    | Not_found => CharMapStateSet.empty
    };

  {
    nfa: {
      start: StateSet.singleton(start),
      finals,
      next,
    },
    annotated: AnnotatedRegex.to_string(annotated),
    nullable,
    firsts: LetterSet.to_string(firsts),
    lasts: LetterSet.to_string(lasts),
    factors: Letter2Set.to_string(factors),
    transitions: StateMapCharSetMapStateSet.to_matrix(joint_transitions)
  };
};

let test = () => {
  let r = RegexParser.parse("a|(b|c)de");
  let glushkov = compile(r);
  assert(StateSet.to_string(glushkov.nfa.finals) == "{1 4}");
  let char_map = glushkov.nfa.next(Int32.zero);
  assert(CharMapStateSet.to_string(char_map) == "{a:{1},b:{2},c:{2}}");
  let char_map = glushkov.nfa.next(Int32.of_int(2));
  assert(CharMapStateSet.to_string(char_map) == "{d:{3}}");
  assert(glushkov.annotated == "(a<sub>1</sub>|[bc]<sub>2</sub>d<sub>3</sub>e<sub>4</sub>)");
  assert(glushkov.nullable == false);
  assert(glushkov.firsts == "a<sub>1</sub> [bc]<sub>2</sub>");
  assert(glushkov.lasts == "a<sub>1</sub> e<sub>4</sub>");
  assert(glushkov.factors == "[bc]<sub>2</sub>d<sub>3</sub> d<sub>3</sub>e<sub>4</sub>");
};
