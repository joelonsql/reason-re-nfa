// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as StateSet$ReasonReNfa from "./StateSet.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";
import * as CharMapStateSet$ReasonReNfa from "./CharMapStateSet.bs.js";

function find_states(sym, nfa, m) {
  try {
    return Curry._2(CharMapStateSet$ReasonReNfa.M[/* find */21], sym, Curry._1(nfa[/* next */2], m));
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return StateSet$ReasonReNfa.S[/* empty */0];
    } else {
      throw exn;
    }
  }
}

function flat_map(f, ss) {
  return Curry._3(StateSet$ReasonReNfa.S[/* fold */13], (function (s) {
                return Curry._1(StateSet$ReasonReNfa.S[/* union */6], Curry._1(f, s));
              }), ss, StateSet$ReasonReNfa.S[/* empty */0]);
}

function nextss(curs, sym, nfa) {
  return flat_map((function (param) {
                return find_states(sym, nfa, param);
              }), curs);
}

function accept(nfa, inp) {
  var _cur = Curry._1(StateSet$ReasonReNfa.S[/* singleton */4], nfa[/* start */0]);
  var _param = inp;
  while(true) {
    var param = _param;
    var cur = _cur;
    if (param) {
      _param = param[1];
      _cur = nextss(cur, param[0], nfa);
      continue ;
    } else {
      return !Curry._1(StateSet$ReasonReNfa.S[/* is_empty */1], Curry._2(StateSet$ReasonReNfa.S[/* inter */7], cur, nfa[/* finals */1]));
    }
  };
}

export {
  find_states ,
  flat_map ,
  nextss ,
  accept ,
  
}
/* StateSet-ReasonReNfa Not a pure module */
