let test = () => {
  print_endline("testing...");
  Nfa.test();
  Range.test();
  RangeSet.test();
  print_endline("OKK!");
  print_endline(Sys.ocaml_version);
};