// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as $$Set from "../node_modules/bs-platform/lib/es6/set.js";
import * as List from "../node_modules/bs-platform/lib/es6/list.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Int32 from "../node_modules/bs-platform/lib/es6/int32.js";
import * as $$String from "../node_modules/bs-platform/lib/es6/string.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";

var S = $$Set.Make([Int32.compare]);

function to_string(state_set) {
  return "{" + ($$String.concat(" ", List.map(Int32.to_string, Curry._1(S[/* elements */19], state_set))) + "}");
}

function example(state_list) {
  return Curry._1(S[/* of_list */25], List.map((function (prim) {
                    return prim;
                  }), state_list));
}

function test(param) {
  if (to_string(example(/* [] */0)) !== "{}") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "StateSet.re",
            13,
            2
          ]
        ];
  }
  if (to_string(example(/* :: */[
              0,
              /* [] */0
            ])) !== "{0}") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "StateSet.re",
            14,
            2
          ]
        ];
  }
  if (to_string(example(/* :: */[
              1,
              /* :: */[
                0,
                /* :: */[
                  2,
                  /* [] */0
                ]
              ]
            ])) === "{0 1 2}") {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "StateSet.re",
            15,
            2
          ]
        ];
  }
}

export {
  S ,
  to_string ,
  example ,
  test ,
  
}
/* S Not a pure module */
