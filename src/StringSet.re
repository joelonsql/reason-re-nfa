include Set.Make(String);

let to_string = string_set => {
  let rec extract_char_set = (char_set, string_set) =>
    fun
    | [] => (char_set, string_set)
    | [s, ...rest] =>
      switch (String.length(s)) {
      | 1 => extract_char_set(CharSet.add(s.[0], char_set), string_set, rest)
      | _ => extract_char_set(char_set, add(s, string_set), rest)
      };
  let (char_set, string_set) =
    extract_char_set(CharSet.empty, empty, elements(string_set));
  let string_set =
    if (CharSet.is_empty(char_set)) {
      string_set;
    } else {
      add(CharSet.to_string(char_set), string_set);
    };
  switch (cardinal(string_set)) {
  | 0 => ""
  | 1 => choose(string_set)
  | _ => "(" ++ String.concat("|", elements(string_set)) ++ ")"
  };
};

let test = () => {
  assert(
    to_string(of_list(["foo", "a", "bar", "b", "x", "c"]))
    == "([a-cx]|bar|foo)",
  );
};