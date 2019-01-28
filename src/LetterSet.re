/** Sets of single letters */;

include Set.Make(Letter);

let (<+>) = union;

let to_string = letter_set =>
  String.concat(" ", List.map(Letter.to_string, elements(letter_set)));

let test = () => {
  let a = singleton((RangeSet.of_char('a'), Int32.zero));
  let bc =
    singleton((RangeSet.singleton(Range.singleton('b', 'c')), Int32.zero));
  let abc = a <+> bc;
  assert(to_string(abc) == "a<sub>0</sub> [bc]<sub>1</sub>");
};