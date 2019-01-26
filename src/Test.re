let test = () => {
  print_endline("testing...");
  CharSetListSet.test();
  Nfa.test();
  print_endline("OK!");
  print_endline(Sys.ocaml_version);
};