let merge_linear =
    (~max_length: int=8, ~max_cardinality: int=32, input_dfa: Dfa.t) => {
  let rec calc_min_length = (src, ranges, cardinality, dst, min_length, seen) =>
    if (Dfa.exists_transition((src, ranges, dst), seen)) {
      (min_length, seen);
    } else {
      let seen = Dfa.add_transition((src, ranges, dst), seen);
      let length = List.length(RangeSetListSet.choose(ranges));
      if (!StateSetSet.mem(dst, input_dfa.finals)
          && length < min_length
          && (
            Dfa.count_children(dst, input_dfa) == 1
            && Dfa.count_parents(dst, input_dfa) == 1
            || cardinality
            * Dfa.count_children(dst, input_dfa) <= max_cardinality
          )) {
        RangeSetListSetMap.fold(
          (ranges', dst', (min_length, seen)) =>
            calc_min_length(
              src,
              RangeSetListSet.merge(ranges, ranges'),
              cardinality * Dfa.count_children(dst, input_dfa),
              dst',
              min_length,
              seen,
            ),
          try (StateSetMap.find(dst, input_dfa.transitions)) {
          | Not_found => RangeSetListSetMap.empty
          },
          (min_length, seen),
        );
      } else {
        (min(length, min_length), seen);
      };
    };

  let calc_min_length_for_state = src => {
    let (min_length, _) =
      RangeSetListSetMap.fold(
        (ranges, dst, (min_length, seen)) =>
          calc_min_length(
            src,
            ranges,
            Dfa.count_children(src, input_dfa),
            dst,
            min_length,
            seen,
          ),
        StateSetMap.find(src, input_dfa.transitions),
        (max_length, Dfa.singleton(src)),
      );
    min_length;
  };

  let rec merge_linear = (src, ranges, dst, dfa, seen) =>
    if (Dfa.exists_transition((src, ranges, dst), seen)) {
      (dfa, seen);
    } else {
      let seen = Dfa.add_transition((src, ranges, dst), seen);
      let (src, ranges, dfa) =
        if (!StateSetSet.mem(dst, input_dfa.finals)
            && List.length(RangeSetListSet.choose(ranges))
            < calc_min_length_for_state(src)) {
          (src, ranges, dfa);
        } else {
          let dfa = Dfa.add_transition((src, ranges, dst), dfa);
          (dst, RangeSetListSet.empty, dfa);
        };
      RangeSetListSetMap.fold(
        (ranges', dst', (dfa, seen)) =>
          merge_linear(
            src,
            RangeSetListSet.merge(ranges, ranges'),
            dst',
            dfa,
            seen,
          ),
        try (StateSetMap.find(dst, input_dfa.transitions)) {
        | Not_found => RangeSetListSetMap.empty
        },
        (dfa, seen),
      );
    };

  let (dfa, _) =
    RangeSetListSetMap.fold(
      (ranges, dst, (dfa, seen)) =>
        merge_linear(input_dfa.start, ranges, dst, dfa, seen),
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