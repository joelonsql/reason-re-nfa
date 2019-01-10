/*
 let compile: string => Dfa.t =
   regex => {
     let r = RegexParser.parse(regex);
     print_endline("ok RegexParser.parse");
     let glushkov = Glushkov.compile(r);
     print_endline("ok Glushkov.compile");
     let nfa = glushkov.nfa;
     let dfa = RabinScott.determinize(nfa);
     print_endline("ok RabinScott.determinize");
     let minimized = Brzozowski.minimize(dfa);
     print_endline("ok Brzozowski.minimize");
     print_endline(Dfa.to_llvm_ir(minimized));
     minimized;
   };

 let accept: (Dfa.t, string) => bool = (dfa, input) => Dfa.accept(dfa, input);

 let to_llvm_ir: Dfa.t => string = dfa => Dfa.to_llvm_ir(dfa);
 */