// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as $$Map from "../node_modules/bs-platform/lib/es6/map.js";
import * as List from "../node_modules/bs-platform/lib/es6/list.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Int32 from "../node_modules/bs-platform/lib/es6/int32.js";
import * as $$String from "../node_modules/bs-platform/lib/es6/string.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";
import * as CharSetMapStateSet$ReasonReNfa from "./CharSetMapStateSet.bs.js";

var M = $$Map.Make([Int32.compare]);

function to_string(state_map) {
  return "{" + ($$String.concat(",", List.rev(Curry._3(M[/* fold */10], (function (i, cs_ss, l) {
                          return /* :: */[
                                  Int32.to_string(i) + (":" + CharSetMapStateSet$ReasonReNfa.to_string(cs_ss)),
                                  l
                                ];
                        }), state_map, /* [] */0))) + "}");
}

function example(state, char_list, state_list) {
  return Curry._2(M[/* singleton */4], state, CharSetMapStateSet$ReasonReNfa.example(char_list, state_list));
}

function test(param) {
  var abc = example(0, /* :: */[
        /* "a" */97,
        /* [] */0
      ], /* :: */[
        1,
        /* [] */0
      ]);
  if (to_string(abc) === "{0:{a:{1}}}") {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "StateMapCharSetMapStateSet.re",
            19,
            2
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
