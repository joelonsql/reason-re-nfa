include Set.Make(RangeSet);

let to_string = ranges_set =>
  "{"
  ++ String.concat(" ", List.map(RangeSet.to_string, elements(ranges_set)))
  ++ "}";

let greatest_common_divisors: t => t =
  input => {
    let aux: (RangeSet.t, t) => t =
      (l, acc) => {
        let l_diff = fold((r, l_diff) => RangeSet.diff(l_diff, r), acc, l);
        let l_inter = RangeSet.diff(l, l_diff);
        let acc =
          fold(
            (r, acc) => {
              let r_diff = RangeSet.diff(r, l_inter);
              if (RangeSet.is_empty(r_diff)) {
                acc;
              } else {
                let acc = remove(r, acc);
                let r_inter = RangeSet.inter(r, l_inter);
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

let factorize: (RangeSet.t, t) => t =
  (ranges, factors) => {
    fold(
      (factor, acc) =>
        if (RangeSet.is_empty(RangeSet.inter(factor, ranges))) {
          acc;
        } else {
          print_endline("factor:" ++ RangeSet.to_string(factor));
          add(RangeSet.set_allow_overlap(false, factor), acc);
        },
      factors,
      empty,
    );
  };

let build_factorize_map: t => RangeSetMap.t(t) =
  input => {
    let gcd = greatest_common_divisors(input);
    fold(
      (ranges, acc) => RangeSetMap.add(ranges, factorize(ranges, gcd), acc),
      input,
      RangeSetMap.empty,
    );
  };