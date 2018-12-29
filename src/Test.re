let test = () => {
  print_endline("testing...");
  let r = RegexParser.parse("0000|0101|1010|1111");
  let glushkov = Glushkov.compile(r);
  let nfa = glushkov.nfa;
  assert(Nfa.accept(nfa, "0101"));
  assert(!Nfa.accept(nfa, "1101"));

  let dfa = RabinScott.determinize(nfa);
  assert(Dfa.accept(dfa, "0101"));
  assert(!Dfa.accept(dfa, "1101"));

  let reversed = Brzozowski.reverse(dfa);

  let dfa2 = RabinScott.determinize(reversed);

  let reversed2 = Brzozowski.reverse(dfa2);

  let dfa_minimal = RabinScott.determinize(reversed2);
  assert(Dfa.accept(dfa_minimal, "0101"));
  assert(!Dfa.accept(dfa_minimal, "0111"));

  print_endline("OK!");
  print_endline(Sys.ocaml_version);
};