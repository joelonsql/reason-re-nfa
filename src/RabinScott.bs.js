// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE

import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Int32 from "../node_modules/bs-platform/lib/es6/int32.js";
import * as Caml_option from "../node_modules/bs-platform/lib/es6/caml_option.js";
import * as Dfa$ReasonReNfa from "./Dfa.bs.js";
import * as Nfa$ReasonReNfa from "./Nfa.bs.js";
import * as CharMap$ReasonReNfa from "./CharMap.bs.js";
import * as CharSet$ReasonReNfa from "./CharSet.bs.js";
import * as StateMap$ReasonReNfa from "./StateMap.bs.js";
import * as StateSet$ReasonReNfa from "./StateSet.bs.js";
import * as CharSetMap$ReasonReNfa from "./CharSetMap.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";
import * as StateSetMap$ReasonReNfa from "./StateSetMap.bs.js";

function flatten_transitions(char_map) {
  return Curry._3(CharSetMap$ReasonReNfa.fold, (function (char_set, state_set, char_map) {
                return Curry._3(CharSet$ReasonReNfa.fold, (function ($$char, char_map) {
                              var entry;
                              try {
                                entry = Curry._2(CharMap$ReasonReNfa.find, $$char, char_map);
                              }
                              catch (exn){
                                if (exn === Caml_builtin_exceptions.not_found) {
                                  entry = StateSet$ReasonReNfa.empty;
                                } else {
                                  throw exn;
                                }
                              }
                              return Curry._3(CharMap$ReasonReNfa.add, $$char, Curry._2(StateSet$ReasonReNfa.union, state_set, entry), char_map);
                            }), char_set, char_map);
              }), char_map, CharMap$ReasonReNfa.empty);
}

function determinize(nfa) {
  var r = /* record */[/* contents */-1];
  var fresh = function (param) {
    r[0] = Int32.succ(r[0]);
    return r[0];
  };
  var build = function (states, param) {
    var finals = param[2];
    var dfa = param[1];
    var map = param[0];
    var exit = 0;
    var state;
    try {
      state = Curry._2(StateSetMap$ReasonReNfa.find, states, map);
      exit = 1;
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        var state$1 = fresh(/* () */0);
        var finals$1 = Curry._1(StateSet$ReasonReNfa.is_empty, Curry._2(StateSet$ReasonReNfa.inter, states, nfa[/* finals */4])) ? finals : Curry._2(StateSet$ReasonReNfa.add, state$1, finals);
        var map$1 = Curry._3(StateSetMap$ReasonReNfa.add, states, state$1, map);
        var match = Curry._3(CharMap$ReasonReNfa.fold, (function (c, ss, param) {
                var state = param[0];
                var match = build(ss, /* tuple */[
                      param[1],
                      param[2],
                      param[3]
                    ]);
                var dfa = Dfa$ReasonReNfa.add_transition(/* tuple */[
                      state,
                      c,
                      match[0]
                    ], match[2]);
                return /* tuple */[
                        state,
                        match[1],
                        dfa,
                        match[3]
                      ];
              }), Curry._3(StateSet$ReasonReNfa.fold, (function (s, m) {
                    var m$prime;
                    try {
                      m$prime = flatten_transitions(Curry._2(StateMap$ReasonReNfa.find, s, nfa[/* transitions */2]));
                    }
                    catch (exn){
                      if (exn === Caml_builtin_exceptions.not_found) {
                        m$prime = CharMap$ReasonReNfa.empty;
                      } else {
                        throw exn;
                      }
                    }
                    return Curry._2(CharMap$ReasonReNfa.union((function (param, s, s$prime) {
                                      return Caml_option.some(Curry._2(StateSet$ReasonReNfa.union, s, s$prime));
                                    })), m, m$prime);
                  }), states, CharMap$ReasonReNfa.empty), /* tuple */[
              state$1,
              map$1,
              dfa,
              finals$1
            ]);
        return /* tuple */[
                match[0],
                match[1],
                match[2],
                match[3]
              ];
      } else {
        throw exn;
      }
    }
    if (exit === 1) {
      return /* tuple */[
              state,
              map,
              dfa,
              finals
            ];
    }
    
  };
  var match = build(nfa[/* start */3], /* tuple */[
        StateSetMap$ReasonReNfa.empty,
        Dfa$ReasonReNfa.singleton(Int32.zero),
        StateSet$ReasonReNfa.empty
      ]);
  return Dfa$ReasonReNfa.set_finals(match[3], match[2]);
}

function test(param) {
  var nfa = Nfa$ReasonReNfa.set_finals(StateSet$ReasonReNfa.example(/* :: */[
            1,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]), Nfa$ReasonReNfa.add_transition(/* tuple */[
            4,
            Curry._1(CharSet$ReasonReNfa.singleton, /* "c" */99),
            4
          ], Nfa$ReasonReNfa.add_transition(/* tuple */[
                3,
                Curry._1(CharSet$ReasonReNfa.singleton, /* "c" */99),
                4
              ], Nfa$ReasonReNfa.add_transition(/* tuple */[
                    2,
                    Curry._1(CharSet$ReasonReNfa.singleton, /* "b" */98),
                    3
                  ], Nfa$ReasonReNfa.add_transition(/* tuple */[
                        0,
                        Curry._1(CharSet$ReasonReNfa.singleton, /* "a" */97),
                        2
                      ], Nfa$ReasonReNfa.add_transition(/* tuple */[
                            0,
                            Curry._1(CharSet$ReasonReNfa.singleton, /* "a" */97),
                            1
                          ], Nfa$ReasonReNfa.singleton(Curry._1(StateSet$ReasonReNfa.singleton, Int32.zero))))))));
  console.log(Nfa$ReasonReNfa.to_matrix(nfa));
  var dfa = determinize(nfa);
  console.log(Dfa$ReasonReNfa.to_matrix(dfa));
  return /* () */0;
}

export {
  flatten_transitions ,
  determinize ,
  test ,
  
}
/* Dfa-ReasonReNfa Not a pure module */
