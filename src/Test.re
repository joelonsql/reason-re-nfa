let test = () => {
  print_endline("testing...");
  Dfa.test();
  Nfa.test();
  CharSet.test();
  let regex = "0000|0101|1010|1111";
  let dfa = Re0.compile(regex);
  let matches_interpreter = Re0.accept(dfa);
  assert(matches_interpreter("0101"));
  assert(!matches_interpreter("1101"));

  let r = RegexParser.parse(".*a..");
  let glushkov = Glushkov.compile(r);
  print_endline(Nfa.to_dot(glushkov.nfa));

  let dfa = Re0.compile(".*a..");
  print_endline(Dfa.to_dot(dfa));

  print_endline("OK!");
  print_endline(Sys.ocaml_version);
};

let () = test();