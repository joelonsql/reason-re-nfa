let analyze = regexp => {
  let parsed = RegexParser.parse(regexp);
  let glushkov = Glushkov.compile(parsed);
  let nfa = glushkov.nfa;
  let dfa = RabinScott.determinize(nfa);
  let nfa' = Brzozowski.dfa_to_nfa(dfa);
  let nfa'' = Brzozowski.reverse(nfa');
  let dfa' = RabinScott.determinize(nfa'');
  let nfa''' = Brzozowski.dfa_to_nfa(dfa');
  let nfa'''' = Brzozowski.reverse(nfa''');
  let dfa'' = RabinScott.determinize(nfa'''');
  let sfa =
    Jakobsson.fold_linear_character_sequences(Brzozowski.dfa_to_sfa(dfa''));
  let sfa2 = Jakobsson.align_strings(sfa);

  /*
     let nfa' = Brzozowski.dfa_to_nfa(dfa);
     let nfa'' = Brzozowski.reverse(nfa');
     let dfa' = RabinScott.determinize(nfa'');
     let nfa''' = Brzozowski.dfa_to_nfa(dfa');
     let nfa'''' = Brzozowski.reverse(nfa''');
     let dfa'' = RabinScott.determinize(nfa'''');
   */

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
    Nfa.to_dot(nfa'),
    Nfa.to_matrix(nfa'),
    Nfa.to_dot(nfa''),
    Nfa.to_matrix(nfa''),
    Dfa.to_dot(dfa'),
    Dfa.to_matrix(dfa'),
    Nfa.to_dot(nfa'''),
    Nfa.to_matrix(nfa'''),
    Nfa.to_dot(nfa''''),
    Nfa.to_matrix(nfa''''),
    Dfa.to_dot(dfa''),
    Dfa.to_matrix(dfa''),
    Sfa.to_dot(sfa),
    Sfa.to_matrix(sfa),
    Sfa.to_dot(sfa2),
    Sfa.to_matrix(sfa2),
    Sfa.to_c(sfa2),
    Sfa.to_llvm_ir(sfa2),
  );
};