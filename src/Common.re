let explode = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [s.[i], ...l]);
    };
  exp(String.length(s) - 1, []);
};

let explode_string = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [String.make(1,s.[i]), ...l]);
    };
  exp(String.length(s) - 1, []);
};

let escaped =
  fun
  | ' '..'~' as c => String.make(1, c)
  | c => Printf.sprintf("\\x%02x", Char.code(c));

let escaped_single_quote =
  fun
  | '\'' => "\\'"
  | '\\' => "\\\\"
  | ' '..'~' as c => String.make(1, c)
  | c => Printf.sprintf("\\x%02x", Char.code(c));

let escaped_double_quote =
  fun
  | '"' => "\\\""
  | '\\' => "\\\\"
  | ' '..'~' as c => String.make(1, c)
  | c => Printf.sprintf("\\\\x%02x", Char.code(c));
