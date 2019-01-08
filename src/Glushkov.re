open Regex;

/** Convert a regex to an ε-free NFA using a slight modification of
    Glushkov's algorithm.

    (The modification: we label character sets rather than characters
     to prevent a state explosion.)
*/;

type t = {
  nfa: Nfa.t,
  annotated: string,
  nullable: bool,
  firsts: string,
  lasts: string,
  factors: string,
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
    | Seq(e, f) =>
      p(e)
      <+> (
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

let positions: LetterSet.t => StateSet.t =
  letter_set =>
    StateSet.of_list(List.map(snd, LetterSet.elements(letter_set)));

let compile: regex('c) => t =
  r => {
    let start: Nfa.state = Int32.zero;
    let annotated = AnnotatedRegex.annotate(r);
    let nullable = l(annotated);
    let firsts = p(annotated);
    let lasts = d(annotated);
    let factors = f_(annotated);
    let nfa =
      Nfa.singleton(StateSet.singleton(start))
      /* Transitions arise from the start state to the initial character sets ... */
      |> LetterSet.fold(
           ((char_set, state)) =>
             CharSet.fold(
               char => Nfa.add_transition((start, char, state)),
               char_set,
             ),
           firsts,
         )
      /* .. and between factors (pairs of character sets with a transition between them) */
      |> Letter2Set.fold(
           (((_, from_state), (char_set, to_state))) =>
             CharSet.fold(
               char => Nfa.add_transition((from_state, char, to_state)),
               char_set,
             ),
           factors,
         )
      /* The final states are the set of 'last' characters in r,
         (+ the start state if r accepts the empty string) */
      |> Nfa.set_finals(
           if (nullable) {
             StateSet.add(start, positions(lasts));
           } else {
             positions(lasts);
           },
         );

    {
      nfa,
      annotated: AnnotatedRegex.to_string(annotated),
      nullable,
      firsts: LetterSet.to_string(firsts),
      lasts: LetterSet.to_string(lasts),
      factors: Letter2Set.to_string(factors),
    };
  };

let test = () => {
  let r = RegexParser.parse("a|(b|c)de");
  let glushkov = compile(r);
  assert(Nfa.accept(glushkov.nfa, "a"));
  assert(Nfa.accept(glushkov.nfa, "bde"));
  assert(!Nfa.accept(glushkov.nfa, "abde"));
};
