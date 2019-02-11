let explode = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [s.[i], ...l]);
    };
  exp(String.length(s) - 1, []);
};

let escaped =
  fun
  | '"' => "%22"
  | '\'' => "%27"
  | '\\' => "%5c"
  | ' '..'~' as c => String.make(1, c)
  | c => Printf.sprintf("%%%02x", Char.code(c));

let escape_string = s => {
  String.concat("", List.map(escaped, explode(s)));
};