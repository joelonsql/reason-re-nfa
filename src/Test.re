let () = {
  print_string("testing...");
  flush(stdout);
  CharSet.test();
  StateSet.test();
  Letter.test();
  LetterSet.test();
  Letter2Set.test();
  CharMapStateSet.test();
  CharSetMapStateSet.test();
  StateMapCharSetMapStateSet.test();
  Glushkov.test();
  Nfa_dot.test();
  print_endline("OK!");
};
