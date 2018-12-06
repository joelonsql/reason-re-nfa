// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as $$Set from "../node_modules/bs-platform/lib/es6/set.js";
import * as List from "../node_modules/bs-platform/lib/es6/list.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Int32 from "../node_modules/bs-platform/lib/es6/int32.js";
import * as $$String from "../node_modules/bs-platform/lib/es6/string.js";
import * as Letter$ReasonReNfa from "./Letter.bs.js";
import * as LetterSet$ReasonReNfa from "./LetterSet.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";

function compare(param, param$1) {
  var i = Int32.compare(param[0][1], param$1[0][1]);
  if (i !== 0) {
    return i;
  } else {
    return Int32.compare(param[1][1], param$1[1][1]);
  }
}

var Pair = /* module */[/* compare */compare];

var include = $$Set.Make(Pair);

var empty = include[0];

var singleton = include[4];

var union = include[6];

var equal = include[10];

var fold = include[13];

function $great$great$eq(m, k) {
  return Curry._3(LetterSet$ReasonReNfa.fold, (function (x, s) {
                return Curry._2(union, Curry._1(k, x), s);
              }), m, empty);
}

function $less$star$great(l, r) {
  return $great$great$eq(l, (function (x) {
                return $great$great$eq(r, (function (y) {
                              return Curry._1(singleton, /* tuple */[
                                          x,
                                          y
                                        ]);
                            }));
              }));
}

function to_string(l2s) {
  return $$String.concat(" ", List.rev(Curry._3(fold, (function (param, l) {
                        return /* :: */[
                                Letter$ReasonReNfa.to_string(param[0]) + Letter$ReasonReNfa.to_string(param[1]),
                                l
                              ];
                      }), l2s, /* [] */0)));
}

function example(letter1, letter2) {
  return Curry._1(singleton, /* tuple */[
              letter1,
              letter2
            ]);
}

function test(param) {
  var letter2 = Letter$ReasonReNfa.example(/* :: */[
        /* "b" */98,
        /* :: */[
          /* "c" */99,
          /* [] */0
        ]
      ], 1);
  var letter1 = Letter$ReasonReNfa.example(/* :: */[
        /* "a" */97,
        /* [] */0
      ], 0);
  var abc = Curry._1(singleton, /* tuple */[
        letter1,
        letter2
      ]);
  if (to_string(abc) !== "a<sub>0</sub>[bc]<sub>1</sub>") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Letter2Set.re",
            44,
            2
          ]
        ];
  }
  var a = LetterSet$ReasonReNfa.example(/* :: */[
        /* "a" */97,
        /* [] */0
      ], 0);
  var bc = LetterSet$ReasonReNfa.example(/* :: */[
        /* "b" */98,
        /* :: */[
          /* "c" */99,
          /* [] */0
        ]
      ], 1);
  var abc$prime = $less$star$great(a, bc);
  if (!Curry._2(equal, abc, abc$prime)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Letter2Set.re",
            48,
            2
          ]
        ];
  }
  var abc$prime$prime = $great$great$eq(a, (function (x) {
          return $great$great$eq(bc, (function (y) {
                        return Curry._1(singleton, /* tuple */[
                                    x,
                                    y
                                  ]);
                      }));
        }));
  if (Curry._2(equal, abc, abc$prime$prime)) {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Letter2Set.re",
            50,
            2
          ]
        ];
  }
}

var is_empty = include[1];

var mem = include[2];

var add = include[3];

var remove = include[5];

var inter = include[7];

var diff = include[8];

var compare$1 = include[9];

var subset = include[11];

var iter = include[12];

var for_all = include[14];

var exists = include[15];

var filter = include[16];

var partition = include[17];

var cardinal = include[18];

var elements = include[19];

var min_elt = include[20];

var max_elt = include[21];

var choose = include[22];

var split = include[23];

var find = include[24];

var of_list = include[25];

var $less$plus$great = union;

export {
  Pair ,
  empty ,
  is_empty ,
  mem ,
  add ,
  singleton ,
  remove ,
  union ,
  inter ,
  diff ,
  compare$1 as compare,
  equal ,
  subset ,
  iter ,
  fold ,
  for_all ,
  exists ,
  filter ,
  partition ,
  cardinal ,
  elements ,
  min_elt ,
  max_elt ,
  choose ,
  split ,
  find ,
  of_list ,
  $less$plus$great ,
  $great$great$eq ,
  $less$star$great ,
  to_string ,
  example ,
  test ,
  
}
/* include Not a pure module */
