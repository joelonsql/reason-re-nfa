// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE

import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Dfa$ReasonReNfa from "./Dfa.bs.js";
import * as StateSet$ReasonReNfa from "./StateSet.bs.js";
import * as RabinScott$ReasonReNfa from "./RabinScott.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";
import * as CharMapStateSet$ReasonReNfa from "./CharMapStateSet.bs.js";
import * as StateMapCharMapStateSet$ReasonReNfa from "./StateMapCharMapStateSet.bs.js";

function reverse(dfa) {
  var map = Dfa$ReasonReNfa.fold_transitions((function (param) {
          var partial_arg_000 = param[2];
          var partial_arg_001 = param[1];
          var partial_arg_002 = param[0];
          var partial_arg = /* tuple */[
            partial_arg_000,
            partial_arg_001,
            partial_arg_002
          ];
          return (function (param) {
              return Dfa$ReasonReNfa.add_transition$prime(partial_arg, param);
            });
        }), dfa, StateMapCharMapStateSet$ReasonReNfa.empty);
  return /* record */[
          /* start */dfa[/* finals */1],
          /* finals */Curry._1(StateSet$ReasonReNfa.singleton, dfa[/* start */0]),
          /* next */(function (s) {
              try {
                return Curry._2(StateMapCharMapStateSet$ReasonReNfa.find, s, map);
              }
              catch (exn){
                if (exn === Caml_builtin_exceptions.not_found) {
                  return CharMapStateSet$ReasonReNfa.empty;
                } else {
                  throw exn;
                }
              }
            })
        ];
}

function minimize(dfa) {
  return RabinScott$ReasonReNfa.determinize(reverse(RabinScott$ReasonReNfa.determinize(reverse(dfa))));
}

function inject(param) {
  var next = param[/* next */2];
  return /* record */[
          /* start */Curry._1(StateSet$ReasonReNfa.singleton, param[/* start */0]),
          /* finals */param[/* finals */1],
          /* next */(function (s) {
              return Curry._2(CharMapStateSet$ReasonReNfa.map, StateSet$ReasonReNfa.singleton, Curry._1(next, s));
            })
        ];
}

export {
  reverse ,
  minimize ,
  inject ,
  
}
/* Dfa-ReasonReNfa Not a pure module */
