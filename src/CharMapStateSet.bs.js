// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as $$Map from "../node_modules/bs-platform/lib/es6/map.js";
import * as Char from "../node_modules/bs-platform/lib/es6/char.js";
import * as List from "../node_modules/bs-platform/lib/es6/list.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as $$String from "../node_modules/bs-platform/lib/es6/string.js";
import * as StateSet$ReasonReNfa from "./StateSet.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";

var M = $$Map.Make([Char.compare]);

function to_string(char_map_state_set) {
  return "{" + ($$String.concat(",", List.map((function (param) {
                      return $$String.make(1, param[0]) + (":" + StateSet$ReasonReNfa.to_string(param[1]));
                    }), Curry._1(M[/* bindings */16], char_map_state_set))) + "}");
}

function example($$char, state_list) {
  return Curry._2(M[/* singleton */4], $$char, StateSet$ReasonReNfa.example(state_list));
}

function test(param) {
  if (to_string(example(/* "a" */97, /* :: */[
              0,
              /* [] */0
            ])) === "{a:{0}}") {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "CharMapStateSet.re",
            15,
            17
          ]
        ];
  }
}

export {
  M ,
  to_string ,
  example ,
  test ,
  
}
/* M Not a pure module */