type t = list(CharSet.t);

let compare = (l, r) => {
  let rec cmp = (l, r) =>
    switch (l, r) {
    | ([], []) => 0
    | ([], _) => (-1)
    | (_, []) => 1
    | ([lh, ...lt], [rh, ...rt]) =>
      switch (CharSet.compare(lh, rh)) {
      | 0 => cmp(lt, rt)
      | n => n
      }
    };
  cmp(l, r);
};

let to_string: t => string =
  char_set_list => {
    String.concat("", List.map(CharSet.to_string, char_set_list));
  };

let of_string: string => t =
  s => {
    List.map(CharSet.singleton, Common.explode(s));
  };