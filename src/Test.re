let test = () => {
  print_endline("testing...");
  StringSet.test();
  Sfa.test();
  let r = RegexParser.parse("a(bcd|bce)f|");
  let glushkov = Glushkov.compile(r);
  print_endline(Nfa.to_dot(glushkov.nfa));
  let dfa = RabinScott.determinize(glushkov.nfa);
  print_endline(Dfa.to_dot(dfa));
  print_endline("OK!");
  print_endline(Sys.ocaml_version);
};