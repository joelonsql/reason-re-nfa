/** A 'letter' is a character set paired with an identifier that
    uniquely identifies the character set within the regex */
type t = (CharSet.S.t, Nfa.state);

let compare = ((_, x), (_, y)) => Pervasives.compare(x, y);

let example = (char_list, state) => (CharSet.example(char_list), Int32.of_int(state));

let to_string =
  fun
  | (char_set, state) => CharSet.to_string(char_set) ++ "<sub>" ++ Int32.to_string(state) ++ "</sub>"
;

let test = () => {
  let a = example(['a'],0);
  let b = example(['b'],0);
  assert(compare(a,b) == 0); /* Equal since only state is compared */
  assert(to_string(a) == "a<sub>0</sub>");
};
