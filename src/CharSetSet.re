include Set.Make(CharSet);

let to_string = char_set_set =>
  "{"
  ++ String.concat(
       " ",
       List.map(CharSet.to_string, elements(char_set_set)),
     )
  ++ "}";

let greatest_common_divisors: t => t =
  input => {
    let aux: (CharSet.t, t) => t =
      (l, acc) => {
        let l_diff = fold((r, l_diff) => CharSet.diff(l_diff, r), acc, l);
        let l_inter = CharSet.diff(l, l_diff);
        let acc =
          fold(
            (r, acc) => {
              let r_diff = CharSet.diff(r, l_inter);
              if (CharSet.is_empty(r_diff)) {
                acc;
              } else {
                let acc = remove(r, acc);
                let r_inter = CharSet.inter(r, l_inter);
                let acc = add(r_diff, acc);
                let acc = add(r_inter, acc);
                acc;
              };
            },
            acc,
            acc,
          );
        add(l_diff, acc);
      };
    fold((l, acc) => aux(l, acc), input, empty);
  };

let factorize: (CharSet.t, t) => t =
  (char_set, factors) => {
    fold(
      (factor, acc) =>
        if (CharSet.is_empty(CharSet.inter(factor, char_set))) {
          acc;
        } else {
          add(factor, acc);
        },
      factors,
      empty,
    );
  };

let build_factorize_map: t => CharSetMap.t(t) =
  input => {
    let gcd = greatest_common_divisors(input);
    fold(
      (char_set, acc) =>
        CharSetMap.add(char_set, factorize(char_set, gcd), acc),
      input,
      CharSetMap.empty,
    );
  };