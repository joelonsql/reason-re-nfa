let analyze = regexp => {
  let parsed = RegexParser.parse(regexp);
  let glushkov = Glushkov.compile(parsed);
  let nfa = glushkov.nfa;
  let dfa = RabinScott.determinize(nfa);

  /** let minimize = dfa => reverse(dfa) -> RabinScott.determinize -> reverse -> RabinScott.determinize; */
  let reversed = Brzozowski.reverse(dfa);
  let dfa2 = RabinScott.determinize(reversed);
  let reversed2 = Brzozowski.reverse(dfa2);
  let dfa_minimal = RabinScott.determinize(reversed2);

  (
    glushkov.nullable,
    glushkov.firsts,
    glushkov.lasts,
    glushkov.factors,
    glushkov.annotated,
    RegexParseTree.of_regex(parsed),
    Nfa.to_dot(nfa),
    Nfa.to_matrix(nfa),
    Dfa.to_dot(dfa),
    Dfa.to_matrix(dfa),
    Nfa.to_dot(reversed),
    Nfa.to_matrix(reversed),
    Dfa.to_dot(dfa2),
    Dfa.to_matrix(dfa2),
    Nfa.to_dot(reversed2),
    Nfa.to_matrix(reversed2),
    Dfa.to_dot(dfa_minimal),
    Dfa.to_matrix(dfa_minimal),
  );
};
