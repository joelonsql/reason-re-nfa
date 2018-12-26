let test = () => {
  print_endline("testing...");
  let r = RegexParser.parse("0000|0101|1010|1111");
  let glushkov = Glushkov.compile(r);
  let nfa = glushkov.nfa;
  Js.log(Nfa.to_dot(nfa));
  Nfa.dump(nfa);
  assert(Nfa.accept(nfa, "0101"));
  assert(!Nfa.accept(nfa, "1101"));

  print_newline();
  Js.log("RabinScott.determinize(nfa)");
  let dfa = RabinScott.determinize(nfa);
  Js.log(Dfa.to_dot(dfa));
  Dfa.dump(dfa);
  assert(Dfa.accept(dfa, "0101"));
  assert(!Dfa.accept(dfa, "1101"));

  print_newline();
  Js.log("Brzozowski.reverse(dfa)");
  let reversed = Brzozowski.reverse(dfa);
  Js.log(Nfa.to_dot(reversed));
  Nfa.dump(reversed);

  print_newline();
  Js.log("RabinScott.determinize(reversed)");
  let dfa2 = RabinScott.determinize(reversed);
  Js.log(Dfa.to_dot(dfa2));
  Dfa.dump(dfa2);

  print_newline();
  Js.log("Brzozowski.reverse(dfa2)");
  let reversed2 = Brzozowski.reverse(dfa2);
  Js.log(Nfa.to_dot(reversed2));
  Nfa.dump(reversed2);

  print_newline();
  Js.log("RabinScott.determinize(reversed2)");
  let dfa_minimal = RabinScott.determinize(reversed2);
  Js.log(Dfa.to_dot(dfa_minimal));
  Dfa.dump(dfa_minimal);

  print_endline("OK!");
  print_endline(Sys.ocaml_version);
};

test();
