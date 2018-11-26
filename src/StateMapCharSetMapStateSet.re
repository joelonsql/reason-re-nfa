module M = Map.Make(Int32);

let to_string = (state_map) => "{" ++ String.concat(
  ",",
  List.rev(M.fold(
    fun (i,cs_ss,l) => [Int32.to_string(i) ++ ":" ++ CharSetMapStateSet.to_string(cs_ss), ...l],
    state_map,
    []
  ))
) ++ "}";

let example = (state, char_list, state_list) => M.singleton(
  Int32.of_int(state),
  CharSetMapStateSet.example(char_list, state_list)
);

let test = () => {
  let abc = example(0, ['a'], [1]);
  assert(to_string(abc) == "{0:{a:{1}}}");
};
