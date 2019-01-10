let analyze = regexp => {
  let parsed = RegexParser.parse(regexp);
  let glushkov = Glushkov.compile(parsed);
  let nfa = glushkov.nfa;

  let nfa = Jakobsson.fold_linear_character_sequences(nfa);
  /*    let nfa = Jakobsson.align_strings(nfa); */

  let dfa = RabinScott.determinize(nfa);
  let nfa' = Brzozowski.dfa_to_nfa(dfa);
  let nfa'' = Brzozowski.reverse(nfa');
  let dfa' = RabinScott.determinize(nfa'');
  let nfa''' = Brzozowski.dfa_to_nfa(dfa');
  let nfa'''' = Brzozowski.reverse(nfa''');
  let dfa'' = RabinScott.determinize(nfa'''');
  /*
     let sdfa =
       Jakobsson.fold_linear_character_sequences(Brzozowski.dfa_to_nfa(dfa''));
     let sdfa2 = Jakobsson.align_strings(sdfa);
   */

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
    Dfa.to_llvm_ir(dfa''),
    /*
     Dfa.to_dot(sdfa),
     Dfa.to_matrix(sdfa),
     Dfa.to_dot(sdfa2),
     Dfa.to_matrix(sdfa2),
     Dfa.to_llvm_ir(sdfa2),
     */
  );
};