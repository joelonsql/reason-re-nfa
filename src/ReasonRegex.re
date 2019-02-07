let analyze = regexp => {
  let parsed = RegexParser.parse(regexp);
  let glushkov = Glushkov.compile(parsed);
  let nfa = glushkov.nfa;
  let dfa = RabinScott.determinize(nfa);
  let nfa2 = Brzozowski.dfa_to_nfa(dfa);
  let nfa3 = Brzozowski.reverse(nfa2);
  let dfa2 = RabinScott.determinize(nfa3);
  let nfa4 = Brzozowski.dfa_to_nfa(dfa2);
  let nfa5 = Brzozowski.reverse(nfa4);
  let dfa3 = RabinScott.determinize(nfa5);
  let dfa4 = Dfa.merge_ranges(dfa3);
  let dfa5 = Dfa.merge_linear(~max_length=8, ~max_cardinality=32, dfa4);
  let dfa6 = Dfa.merge_branches(dfa5);
  let nfa6 = Brzozowski.dfa_to_nfa(dfa6);
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
    Nfa.to_dot(nfa2),
    Nfa.to_matrix(nfa2),
    Nfa.to_dot(nfa3),
    Nfa.to_matrix(nfa3),
    Dfa.to_dot(dfa2),
    Dfa.to_matrix(dfa2),
    Nfa.to_dot(nfa4),
    Nfa.to_matrix(nfa4),
    Nfa.to_dot(nfa5),
    Nfa.to_matrix(nfa5),
    Dfa.to_dot(dfa3),
    Dfa.to_matrix(dfa3),
    Dfa.to_dot(dfa4),
    Dfa.to_matrix(dfa4),
    Dfa.to_dot(dfa5),
    Dfa.to_matrix(dfa5),
    Dfa.to_dot(dfa6),
    Dfa.to_matrix(dfa6),
    Nfa.to_dot(nfa6),
    Nfa.to_matrix(nfa6),
    "",
    "",
    /*
     Dfa.to_llvm_ir(dfa5),
     Dfa.to_js(Dfa.SingleEntry, dfa5),
     */
  );
};