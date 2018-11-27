// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as List from "../node_modules/bs-platform/lib/es6/list.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Int32 from "../node_modules/bs-platform/lib/es6/int32.js";
import * as CharSet$ReasonReNfa from "./CharSet.bs.js";
import * as StateSet$ReasonReNfa from "./StateSet.bs.js";
import * as LetterSet$ReasonReNfa from "./LetterSet.bs.js";
import * as Letter2Set$ReasonReNfa from "./Letter2Set.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";
import * as RegexParser$ReasonReNfa from "./RegexParser.bs.js";
import * as AnnotatedRegex$ReasonReNfa from "./AnnotatedRegex.bs.js";
import * as CharMapStateSet$ReasonReNfa from "./CharMapStateSet.bs.js";
import * as CharSetMapStateSet$ReasonReNfa from "./CharSetMapStateSet.bs.js";
import * as StateMapCharSetMapStateSet$ReasonReNfa from "./StateMapCharSetMapStateSet.bs.js";

function l(_param) {
  while(true) {
    var param = _param;
    if (typeof param === "number") {
      if (param === 0) {
        return false;
      } else {
        return true;
      }
    } else {
      switch (param.tag | 0) {
        case 0 : 
            return false;
        case 1 : 
            if (l(param[0])) {
              return true;
            } else {
              _param = param[1];
              continue ;
            }
        case 2 : 
            if (l(param[0])) {
              _param = param[1];
              continue ;
            } else {
              return false;
            }
        case 3 : 
            return true;
        
      }
    }
  };
}

function p(_param) {
  while(true) {
    var param = _param;
    if (typeof param === "number") {
      return LetterSet$ReasonReNfa.S[/* empty */0];
    } else {
      switch (param.tag | 0) {
        case 0 : 
            return Curry._1(LetterSet$ReasonReNfa.S[/* singleton */4], param[0]);
        case 1 : 
            return Curry._2(LetterSet$ReasonReNfa.$less$plus$great, p(param[0]), p(param[1]));
        case 2 : 
            var e = param[0];
            return Curry._2(LetterSet$ReasonReNfa.$less$plus$great, p(e), l(e) ? p(param[1]) : LetterSet$ReasonReNfa.S[/* empty */0]);
        case 3 : 
            _param = param[0];
            continue ;
        
      }
    }
  };
}

function d(_param) {
  while(true) {
    var param = _param;
    if (typeof param === "number") {
      return LetterSet$ReasonReNfa.S[/* empty */0];
    } else {
      switch (param.tag | 0) {
        case 0 : 
            return Curry._1(LetterSet$ReasonReNfa.S[/* singleton */4], param[0]);
        case 1 : 
            return Curry._2(LetterSet$ReasonReNfa.$less$plus$great, d(param[0]), d(param[1]));
        case 2 : 
            var e = param[1];
            return Curry._2(LetterSet$ReasonReNfa.$less$plus$great, l(e) ? d(param[0]) : LetterSet$ReasonReNfa.S[/* empty */0], d(e));
        case 3 : 
            _param = param[0];
            continue ;
        
      }
    }
  };
}

function f_(param) {
  if (typeof param === "number") {
    return Letter2Set$ReasonReNfa.S[/* empty */0];
  } else {
    switch (param.tag | 0) {
      case 1 : 
          return Curry._2(Letter2Set$ReasonReNfa.$less$plus$great, f_(param[0]), f_(param[1]));
      case 2 : 
          var f = param[1];
          var e = param[0];
          return Curry._2(Letter2Set$ReasonReNfa.$less$plus$great, Curry._2(Letter2Set$ReasonReNfa.$less$plus$great, f_(e), f_(f)), Letter2Set$ReasonReNfa.$less$star$great(d(e), p(f)));
      case 3 : 
          var e$1 = param[0];
          return Curry._2(Letter2Set$ReasonReNfa.$less$plus$great, f_(e$1), Letter2Set$ReasonReNfa.$less$star$great(d(e$1), p(e$1)));
      default:
        return Letter2Set$ReasonReNfa.S[/* empty */0];
    }
  }
}

function transition_map_of_factor_set(factors) {
  return Curry._3(Letter2Set$ReasonReNfa.S[/* fold */13], (function (param, state_map) {
                var match = param[1];
                var from_state = param[0][1];
                var char_set = match[0];
                var to_state = match[1];
                var state_map$1 = state_map;
                var char_set_map;
                try {
                  char_set_map = Curry._2(StateMapCharSetMapStateSet$ReasonReNfa.M[/* find */21], from_state, state_map$1);
                }
                catch (exn){
                  if (exn === Caml_builtin_exceptions.not_found) {
                    char_set_map = CharSetMapStateSet$ReasonReNfa.M[/* empty */0];
                  } else {
                    throw exn;
                  }
                }
                var state_set;
                try {
                  state_set = Curry._2(CharSetMapStateSet$ReasonReNfa.M[/* find */21], char_set, char_set_map);
                }
                catch (exn$1){
                  if (exn$1 === Caml_builtin_exceptions.not_found) {
                    state_set = StateSet$ReasonReNfa.S[/* empty */0];
                  } else {
                    throw exn$1;
                  }
                }
                var state_set$1 = Curry._2(StateSet$ReasonReNfa.S[/* add */3], to_state, state_set);
                var char_set_map$1 = Curry._3(CharSetMapStateSet$ReasonReNfa.M[/* add */3], char_set, state_set$1, char_set_map);
                return Curry._3(StateMapCharSetMapStateSet$ReasonReNfa.M[/* add */3], from_state, char_set_map$1, state_map$1);
              }), factors, StateMapCharSetMapStateSet$ReasonReNfa.M[/* empty */0]);
}

function positions(letter_set) {
  return Curry._1(StateSet$ReasonReNfa.S[/* of_list */25], List.map((function (prim) {
                    return prim[1];
                  }), Curry._1(LetterSet$ReasonReNfa.S[/* elements */19], letter_set)));
}

function transition_map_of_letter_set(letter_set) {
  return Curry._3(LetterSet$ReasonReNfa.S[/* fold */13], (function (param, char_set_map) {
                var state = param[1];
                var char_set = param[0];
                var entry;
                var exit = 0;
                var state_set;
                try {
                  state_set = Curry._2(CharSetMapStateSet$ReasonReNfa.M[/* find */21], char_set, char_set_map);
                  exit = 1;
                }
                catch (exn){
                  if (exn === Caml_builtin_exceptions.not_found) {
                    entry = Curry._1(StateSet$ReasonReNfa.S[/* singleton */4], state);
                  } else {
                    throw exn;
                  }
                }
                if (exit === 1) {
                  entry = Curry._2(StateSet$ReasonReNfa.S[/* add */3], state, state_set);
                }
                return Curry._3(CharSetMapStateSet$ReasonReNfa.M[/* add */3], char_set, entry, char_set_map);
              }), letter_set, CharSetMapStateSet$ReasonReNfa.M[/* empty */0]);
}

function flatten_transitions(char_map) {
  return Curry._3(CharSetMapStateSet$ReasonReNfa.M[/* fold */10], (function (char_set, state_set, char_map) {
                return Curry._3(CharSet$ReasonReNfa.S[/* fold */13], (function ($$char, char_map) {
                              var entry;
                              try {
                                entry = Curry._2(CharMapStateSet$ReasonReNfa.M[/* find */21], $$char, char_map);
                              }
                              catch (exn){
                                if (exn === Caml_builtin_exceptions.not_found) {
                                  entry = StateSet$ReasonReNfa.S[/* empty */0];
                                } else {
                                  throw exn;
                                }
                              }
                              return Curry._3(CharMapStateSet$ReasonReNfa.M[/* add */3], $$char, Curry._2(StateSet$ReasonReNfa.S[/* union */6], state_set, entry), char_map);
                            }), char_set, char_map);
              }), char_map, CharMapStateSet$ReasonReNfa.M[/* empty */0]);
}

function compile(r) {
  var annotated = AnnotatedRegex$ReasonReNfa.annotate(r);
  var nullable = l(annotated);
  var firsts = p(annotated);
  var lasts = d(annotated);
  var factors = f_(annotated);
  var finals = nullable ? Curry._2(StateSet$ReasonReNfa.S[/* add */3], Int32.zero, positions(lasts)) : positions(lasts);
  var factor_transitions = transition_map_of_factor_set(factors);
  var initial_transitions = transition_map_of_letter_set(firsts);
  var joint_transitions = Curry._3(StateMapCharSetMapStateSet$ReasonReNfa.M[/* add */3], Int32.zero, initial_transitions, factor_transitions);
  var next = function (state) {
    try {
      return flatten_transitions(Curry._2(StateMapCharSetMapStateSet$ReasonReNfa.M[/* find */21], state, joint_transitions));
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        return CharMapStateSet$ReasonReNfa.M[/* empty */0];
      } else {
        throw exn;
      }
    }
  };
  return /* record */[
          /* nfa : record */[
            /* start */Int32.zero,
            /* finals */finals,
            /* next */next
          ],
          /* annotated */AnnotatedRegex$ReasonReNfa.to_string(annotated),
          /* nullable */nullable,
          /* firsts */LetterSet$ReasonReNfa.to_string(firsts),
          /* lasts */LetterSet$ReasonReNfa.to_string(lasts),
          /* factors */Letter2Set$ReasonReNfa.to_string(factors),
          /* factor_transitions */StateMapCharSetMapStateSet$ReasonReNfa.to_matrix(factor_transitions),
          /* initial_transitions */CharSetMapStateSet$ReasonReNfa.to_string(initial_transitions),
          /* joint_transitions */StateMapCharSetMapStateSet$ReasonReNfa.to_matrix(joint_transitions)
        ];
}

function test(param) {
  var r = RegexParser$ReasonReNfa.parse("a|(b|c)de");
  var glushkov = compile(r);
  if (StateSet$ReasonReNfa.to_string(glushkov[/* nfa */0][/* finals */1]) !== "{1 4}") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            201,
            2
          ]
        ];
  }
  var char_map = Curry._1(glushkov[/* nfa */0][/* next */2], Int32.zero);
  if (CharMapStateSet$ReasonReNfa.to_string(char_map) !== "{a:{1},b:{2},c:{2}}") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            203,
            2
          ]
        ];
  }
  var char_map$1 = Curry._1(glushkov[/* nfa */0][/* next */2], 2);
  if (CharMapStateSet$ReasonReNfa.to_string(char_map$1) !== "{d:{3}}") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            205,
            2
          ]
        ];
  }
  if (glushkov[/* annotated */1] !== "a<sub>1</sub> {b c}<sub>2</sub> d<sub>3</sub> e<sub>4</sub> ") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            206,
            2
          ]
        ];
  }
  if (glushkov[/* nullable */2] !== false) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            207,
            2
          ]
        ];
  }
  if (glushkov[/* firsts */3] !== "a<sub>1</sub> {b c}<sub>2</sub>") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            208,
            2
          ]
        ];
  }
  if (glushkov[/* lasts */4] !== "a<sub>1</sub> e<sub>4</sub>") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            209,
            2
          ]
        ];
  }
  if (glushkov[/* factors */5] !== "{b c}<sub>2</sub>d<sub>3</sub> d<sub>3</sub>e<sub>4</sub>") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            210,
            2
          ]
        ];
  }
  if (glushkov[/* initial_transitions */7] === "{a:{1},{b c}:{2}}") {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Glushkov.re",
            211,
            2
          ]
        ];
  }
}

export {
  l ,
  p ,
  d ,
  f_ ,
  transition_map_of_factor_set ,
  positions ,
  transition_map_of_letter_set ,
  flatten_transitions ,
  compile ,
  test ,
  
}
/* CharSet-ReasonReNfa Not a pure module */
