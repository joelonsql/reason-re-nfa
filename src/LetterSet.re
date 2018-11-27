/** Sets of single letters */
module S = Set.Make(Letter);

let (<+>) = S.union;

let to_string = (letter_set) => {
  String.concat(
    " ",
    List.map(Letter.to_string, S.elements(letter_set))
  );
};

let example = (char_list, state) =>
  S.singleton(Letter.example(char_list, state));

let test = () => {
  let a = example(['a'],0);
  let bc = example(['b','c'],1);
  let abc = a <+> bc;
  assert(to_string(abc) == "a<sub>0</sub> {b c}<sub>1</sub>");
};
