// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as $$Set from "../node_modules/bs-platform/lib/es6/set.js";
import * as List from "../node_modules/bs-platform/lib/es6/list.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as $$String from "../node_modules/bs-platform/lib/es6/string.js";
import * as Letter$ReasonReNfa from "./Letter.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";

var S = $$Set.Make([Letter$ReasonReNfa.compare]);

var $less$plus$great = S[/* union */6];

function to_string(letter_set) {
  return $$String.concat(" ", List.map(Letter$ReasonReNfa.to_string, Curry._1(S[/* elements */19], letter_set)));
}

function example(char_list, state) {
  return Curry._1(S[/* singleton */4], Letter$ReasonReNfa.example(char_list, state));
}

function test(param) {
  var a = example(/* :: */[
        /* "a" */97,
        /* [] */0
      ], 0);
  var bc = example(/* :: */[
        /* "b" */98,
        /* :: */[
          /* "c" */99,
          /* [] */0
        ]
      ], 1);
  var abc = Curry._2($less$plus$great, a, bc);
  if (to_string(abc) === "a<sub>0</sub> {b c}<sub>1</sub>") {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "LetterSet.re",
            20,
            2
          ]
        ];
  }
}

export {
  S ,
  $less$plus$great ,
  to_string ,
  example ,
  test ,
  
}
/* S Not a pure module */