module M = Map.Make(Int32);

let to_string = (state_map) =>
  "{" ++ String.concat(
    ",",
    List.rev(M.fold(
      fun (state,char_set,l) => [Int32.to_string(state) ++ ":" ++ CharSetMapStateSet.to_string(char_set), ...l],
      state_map,
      []
   ))
  ) ++ "}";

let example = (state, char_list, state_list) =>
  M.singleton(
    Int32.of_int(state),
    CharSetMapStateSet.example(char_list, state_list)
  );

let test = () => {
  let abc = example(0, ['a'], [1]);
  assert(to_string(abc) == "{0:{a:{1}}}");
};

let to_matrix = (state_map) => {
  module CharSetSet = Set.Make(CharSet.S);
  let char_sets =
    Array.of_list(
      CharSetSet.elements(
        M.fold(
          fun (_, char_set_map, char_set_set) =>
            CharSetSet.union(
              CharSetSet.of_list(
                List.map(
                  fun ((char_set, _)) => char_set,
                  CharSetMapStateSet.M.bindings(char_set_map)
                )
              ),
              char_set_set
            ),
          state_map,
          CharSetSet.empty
        )
      )
    );

  let states =
    Array.of_list(
      List.map(
        fun ((state,_)) => state,
        M.bindings(state_map)
      )
    );

  let dimx = Array.length(states);
  let dimy = Array.length(char_sets);
  let matrix = Array.make_matrix(dimx+1, dimy+1, "");
  for (x in 1 to dimx) {
    matrix[x][0] = Int32.to_string(states[x-1]);
    for (y in 1 to dimy) {
      if (x == 1) {
        matrix[0][y] = CharSet.to_string(char_sets[y-1]);
      }
      matrix[x][y] =
        switch (CharSetMapStateSet.M.find(char_sets[y-1], M.find(states[x-1], state_map))) {
        | exception Not_found => ""
        | state_set => StateSet.to_string(state_set)
        };
    }
  }
  matrix;
};
