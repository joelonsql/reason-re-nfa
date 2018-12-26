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
