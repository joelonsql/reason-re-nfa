include Map.Make(Char);

/* val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t */
let union = (type a, f: (char, a, a) => option(a)) => {
  let f = (k, x, y) =>
    switch (x, y) {
    | (None, None) => None
    | (Some(v), None) => Some(v)
    | (None, Some(v)) => Some(v)
    | (Some(v1), Some(v2)) => f(k, v1, v2)
    };
  merge(f);
};

let to_string = (char_map_state_set) =>
  "{" ++ String.concat(
    ",",
    List.map(
      fun ((c,ss)) => String.make(1,c) ++ ":" ++ StateSet.to_string(ss),
      bindings(char_map_state_set)
    )
  ) ++ "}";

let example = (char, state_list) =>
  singleton(char, StateSet.example(state_list));

let test = () => {
  assert(to_string(example('a',[0])) == "{a:{0}}");
};
