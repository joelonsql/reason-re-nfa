let merge_linear =
    (~max_length: int=8, ~max_cardinality: int=32, input_dfa: Dfa.t) => {
  let rec merge_linear = (src, ranges, cardinality, dst, dfa, seen, lengths) =>
    if (List.length(RangeSetListSet.choose(ranges)) > 0
        && Dfa.exists_transition((src, ranges, dst), seen)) {
      (dfa, seen);
    } else {
      let seen = Dfa.add_transition((src, ranges, dst), seen);
      let (src, ranges, cardinality, dfa) =
        if (!StateSetSet.mem(dst, input_dfa.finals)
            && List.length(RangeSetListSet.choose(ranges))
            < (
                switch (StateSetMap.find(src, lengths)) {
                | exception Not_found => max_length
                | length => min(length, max_length)
                }
              )
            && (
              Dfa.count_children(dst, input_dfa) == 1
              && Dfa.count_parents(dst, input_dfa) == 1
              || cardinality
              * Dfa.count_children(dst, input_dfa) <= max_cardinality
            )) {
          (
            src,
            ranges,
            cardinality * Dfa.count_children(dst, input_dfa),
            dfa,
          );
        } else {
          let dfa = Dfa.add_transition((src, ranges, dst), dfa);
          (
            dst,
            RangeSetListSet.empty,
            Dfa.count_children(dst, input_dfa),
            dfa,
          );
        };
      RangeSetListSetMap.fold(
        (ranges', dst', (dfa, seen)) =>
          merge_linear(
            src,
            RangeSetListSet.merge(ranges, ranges'),
            cardinality,
            dst',
            dfa,
            seen,
            lengths,
          ),
        try (StateSetMap.find(dst, input_dfa.transitions)) {
        | Not_found => RangeSetListSetMap.empty
        },
        (dfa, seen),
      );
    };

  let length_dfa = {
    let (dfa, _) =
      RangeSetListSetMap.fold(
        (ranges, dst, (dfa, seen)) =>
          merge_linear(
            input_dfa.start,
            ranges,
            Dfa.count_children(input_dfa.start, input_dfa),
            dst,
            dfa,
            seen,
            StateSetMap.empty,
          ),
        StateSetMap.find(input_dfa.start, input_dfa.transitions),
        (Dfa.singleton(input_dfa.start), Dfa.singleton(input_dfa.start)),
      );
    Dfa.set_finals(input_dfa.finals, dfa);
  };

  let length_map =
    StateSetMap.fold(
      (src, ranges, length_map) =>
        RangeSetListSetMap.fold(
          (ranges, _, length_map) => {
            let length = List.length(RangeSetListSet.choose(ranges));
            let cur_length =
              try (StateSetMap.find(src, length_map)) {
              | Not_found => length
              };
            StateSetMap.add(src, min(length, cur_length), length_map);
          },
          ranges,
          length_map,
        ),
      length_dfa.transitions,
      StateSetMap.empty,
    );

  let (dfa, _) =
    RangeSetListSetMap.fold(
      (ranges, dst, (dfa, seen)) =>
        merge_linear(
          input_dfa.start,
          ranges,
          Dfa.count_children(input_dfa.start, input_dfa),
          dst,
          dfa,
          seen,
          length_map,
        ),
      StateSetMap.find(input_dfa.start, input_dfa.transitions),
      (Dfa.singleton(input_dfa.start), Dfa.singleton(input_dfa.start)),
    );
  Dfa.set_finals(input_dfa.finals, dfa);
};

let merge_branches: Dfa.t => Dfa.t =
  input_dfa => {
    let group_by: Dfa.transitions => Dfa.transitions =
      transitions => {
        let fold_ranges_map:
          RangeSetListSetMap.t(Dfa.state) => StateSetMap.t(RangeSetListSet.t) =
          ranges_map =>
            RangeSetListSetMap.fold(
              (ranges, dst, dst_map) => {
                let ranges' =
                  switch (StateSetMap.find(dst, dst_map)) {
                  | exception Not_found => ranges
                  | ranges' => RangeSetListSet.union(ranges', ranges)
                  };
                let dst_map = StateSetMap.add(dst, ranges', dst_map);
                dst_map;
              },
              ranges_map,
              StateSetMap.empty,
            );

        StateSetMap.fold(
          (src, ranges_map, acc) =>
            StateSetMap.fold(
              (dst, ranges, acc) =>
                StateSetMap.add(
                  src,
                  switch (StateSetMap.find(src, acc)) {
                  | exception Not_found =>
                    RangeSetListSetMap.singleton(ranges, dst)
                  | ranges_map =>
                    RangeSetListSetMap.add(ranges, dst, ranges_map)
                  },
                  acc,
                ),
              fold_ranges_map(ranges_map),
              acc,
            ),
          transitions,
          StateSetMap.empty,
        );
      };

    StateSetMap.fold(
      (src, ranges_map, dfa) =>
        RangeSetListSetMap.fold(
          (ranges, dst, dfa) => Dfa.add_transition((src, ranges, dst), dfa),
          ranges_map,
          dfa,
        ),
      group_by(input_dfa.transitions),
      Dfa.singleton(input_dfa.start),
    )
    |> Dfa.set_finals(input_dfa.finals);
  };

let merge_ranges: Dfa.t => Dfa.t =
  input_dfa => {
    let group_by: Dfa.transitions => Dfa.transitions =
      transitions => {
        let fold_ranges_map:
          RangeSetListSetMap.t(Dfa.state) => StateSetMap.t(RangeSetListSet.t) =
          ranges_map =>
            RangeSetListSetMap.fold(
              (ranges, dst, dst_map) =>
                StateSetMap.add(
                  dst,
                  switch (StateSetMap.find(dst, dst_map)) {
                  | exception Not_found => ranges
                  | ranges' =>
                    RangeSetListSet.singleton([
                      RangeSet.union(
                        RangeSetListSet.choose_strict(ranges),
                        RangeSetListSet.choose_strict(ranges'),
                      ),
                    ])
                  },
                  dst_map,
                ),
              ranges_map,
              StateSetMap.empty,
            );

        StateSetMap.fold(
          (src, ranges_map, acc) =>
            StateSetMap.fold(
              (dst, ranges, acc) =>
                StateSetMap.add(
                  src,
                  switch (StateSetMap.find(src, acc)) {
                  | exception Not_found =>
                    RangeSetListSetMap.singleton(ranges, dst)
                  | ranges_map =>
                    RangeSetListSetMap.add(ranges, dst, ranges_map)
                  },
                  acc,
                ),
              fold_ranges_map(ranges_map),
              acc,
            ),
          transitions,
          StateSetMap.empty,
        );
      };

    StateSetMap.fold(
      (src, ranges_map, dfa) =>
        RangeSetListSetMap.fold(
          (ranges, dst, dfa) => Dfa.add_transition((src, ranges, dst), dfa),
          ranges_map,
          dfa,
        ),
      group_by(input_dfa.transitions),
      Dfa.singleton(input_dfa.start),
    )
    |> Dfa.set_finals(input_dfa.finals);
  };