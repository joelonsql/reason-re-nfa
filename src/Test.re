let test = () => {
  print_endline("testing...");
  Nfa.test();
  Range.test();
  RangeSet.test();
  print_endline(
    String.make(1, Char.chr(int_of_string("0xe2")))
    ++ String.make(1, Char.chr(int_of_string("0x82")))
    ++ String.make(1, Char.chr(int_of_string("0xac"))),
  );
  print_endline("€");
  let s =
    String.make(1, Char.chr(int_of_string("0xe2")))
    ++ String.make(1, Char.chr(int_of_string("0x82")))
    ++ String.make(1, Char.chr(int_of_string("0xac")));
  Js.log({j|$s|j});
  RangeSetList.test();
  RangeSetListSet.test();
  print_endline("OK!");
  print_endline(Sys.ocaml_version);
};