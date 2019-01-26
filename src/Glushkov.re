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

let split: CharSetSet.t => CharSetSet.t =
  input => {
    let aux: (CharSet.t, CharSetSet.t) => CharSetSet.t =
      (l, acc) => {
        let l_rest =
          CharSetSet.fold((r, l_rest) => CharSet.diff(l_rest, r), acc, l);
        CharSetSet.add(l_rest, acc);
      };
    CharSetSet.fold((l, acc) => aux(l, acc), input, CharSetSet.empty);
  };

let factor_char_set_set: CharSetSet.t => CharSetSet.t =
  input => {
    let aux: (CharSet.t, CharSetSet.t) => CharSetSet.t =
      (l, acc) => {
        /** Remove characters in common with existing set of atom-charsets,
        this will give us the new characters not present in the current set */
        let l_diff =
          CharSetSet.fold((r, l_diff) => CharSet.diff(l_diff, r), acc, l);
        /** Get the characters we already had in common,
            which is the difference of the difference */
        let l_inter = CharSet.diff(l, l_diff);
        /** Of these, are there any existing atom-charsets which are not fully covered by us
            if so, these must be split into smaller atoms */
        let acc =
          CharSetSet.fold(
            (r, acc) => {
              let r_diff = CharSet.diff(r, l_inter);
              if (CharSet.is_empty(r_diff)) {
                acc;
              } else {
                let acc = CharSetSet.remove(r, acc);
                let r_inter = CharSet.inter(r, l_inter);
                let acc = CharSetSet.add(r_diff, acc);
                let acc = CharSetSet.add(r_inter, acc);
                acc;
              };
            },
            acc,
            acc,
          );
        let acc = CharSetSet.add(l_diff, acc);
        /** Add these new characters to the set */ acc;
      };
    CharSetSet.fold((l, acc) => aux(l, acc), input, CharSetSet.empty);
  };

let compile: regex('c) => t =
  r => {
    let start: Nfa.state = Int32.zero;
    let annotated = AnnotatedRegex.annotate(r);
    let nullable = l(annotated);
    let firsts = p(annotated);
    let lasts = d(annotated);
    let factors = f_(annotated);

    let char_set_set =
      LetterSet.fold(
        ((char_set, _), char_set_set) =>
          CharSetSet.add(char_set, char_set_set),
        firsts,
        CharSetSet.empty,
      )
      |> Letter2Set.fold(
           (((_, _), (char_set, _)), char_set_set) =>
             CharSetSet.add(char_set, char_set_set),
           factors,
         );
    print_endline(CharSetSet.to_string(char_set_set));
    let gcd = CharSetSet.greatest_common_divisors(char_set_set);

    print_endline("gcd: " ++ CharSetSet.to_string(gcd));

    let factorize_map = CharSetSet.build_factorize_map(char_set_set);

    print_endline("factorize_map: ");
    CharSetMap.iter(
      (char_set, char_set_set) =>
        print_endline(
          CharSet.to_string(char_set)
          ++ " : "
          ++ CharSetSet.to_string(char_set_set),
        ),
      factorize_map,
    );

    let nfa =
      Nfa.singleton(StateSet.singleton(start))
      /* Transitions arise from the start state to the initial character sets ... */
      |> LetterSet.fold(
           ((char_set, state)) =>
             CharSetSet.fold(
               char_set => {
                 print_endline(
                   "Glushkov initial: "
                   ++ Int32.to_string(start)
                   ++ " "
                   ++ CharSet.to_string(char_set)
                   ++ " "
                   ++ Int32.to_string(state),
                 );
                 Nfa.add_transition((start, [char_set], state));
               },
               CharSetMap.find(char_set, factorize_map),
             ),
           firsts,
         )
      /* .. and between factors (pairs of character sets with a transition between them) */
      |> Letter2Set.fold(
           (((_, from_state), (char_set, to_state))) =>
             CharSetSet.fold(
               char_set => {
                 print_endline(
                   "Glushkov factors: "
                   ++ Int32.to_string(from_state)
                   ++ " "
                   ++ CharSet.to_string(char_set)
                   ++ " "
                   ++ Int32.to_string(to_state),
                 );
                 Nfa.add_transition((from_state, [char_set], to_state));
               },
               CharSetMap.find(char_set, factorize_map),
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