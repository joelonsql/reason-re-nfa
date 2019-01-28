/** A 'letter' is a character set paired with an identifier that
    uniquely identifies the character set within the regex */

type t = (RangeSet.t, Nfa.state);

let compare = ((_, x), (_, y)) => Int32.compare(x, y);

let to_string =
  fun
  | (range_set, state) =>
    RangeSet.to_string(range_set)
    ++ "<sub>"
    ++ Int32.to_string(state)
    ++ "</sub>";

let test = () => {
  let a = (RangeSet.of_char('a'), Int32.zero);
  let b = (RangeSet.of_char('b'), Int32.zero);
  assert(compare(a, b) == 0);
  assert(to_string(a) == "a<sub>0</sub>");
};