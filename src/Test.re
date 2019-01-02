let test = () => {
  print_endline("testing...");
  let r = RegexParser.parse("a(bcd|bce)f");
  let glushkov = Glushkov.compile(r);
  print_endline(Nfa.to_dot(glushkov.nfa));
  print_endline(Dfa.to_dot(RabinScott.determinize(glushkov.nfa)));
  print_endline("OK!");
  print_endline(Sys.ocaml_version);
};