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
      exp(i - 1, [String.make(1, s.[i]), ...l]);
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

let encode_string_as_int_or_vector = s => {
  let rec exp = (pos, i) =>
    if (pos < 0) {
      i;
    } else {
      exp(
        pos - 1,
        Int64.add(
          i,
          Int64.shift_left(Int64.of_int(Char.code(s.[pos])), 8 * pos),
        ),
      );
    };
  let length = String.length(s);
  if (length <= 8) {
    Int64.to_string(exp(String.length(s) - 1, Int64.zero));
  } else {
    "<"
    ++ String.concat(
         ",",
         List.map(
           chr => " i8 " ++ string_of_int(Char.code(chr)),
           explode(s),
         ),
       )
    ++ " >";
  };
};

let escape_string = s => {
  String.concat("", List.map(escaped, explode(s)));
};