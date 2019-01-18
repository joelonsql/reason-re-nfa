include Map.Make(Int32);

exception NoElement(string);
exception MultipleElements(string);

let choose_strict = str_len_map =>
  switch (cardinal(str_len_map)) {
  | 1 => choose(str_len_map)
  | 0 => raise(NoElement("Expected 1 element, got 0"))
  | n =>
    raise(MultipleElements("Expected 1 element, got " ++ string_of_int(n)))
  };