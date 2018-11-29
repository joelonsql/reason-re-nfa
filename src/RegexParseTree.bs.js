// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE

import * as Block from "../node_modules/bs-platform/lib/es6/block.js";
import * as CharSet$ReasonReNfa from "./CharSet.bs.js";

function of_regex(r) {
  if (typeof r === "number") {
    if (r === 0) {
      return /* Leaf */Block.__(2, ["Empty"]);
    } else {
      return /* Leaf */Block.__(2, ["Eps"]);
    }
  } else {
    switch (r.tag | 0) {
      case 0 : 
          return /* One */Block.__(0, [
                    "Char",
                    /* Leaf */Block.__(2, [CharSet$ReasonReNfa.to_string(r[0])])
                  ]);
      case 1 : 
          return /* Two */Block.__(1, [
                    "Alt",
                    of_regex(r[0]),
                    of_regex(r[1])
                  ]);
      case 2 : 
          return /* Two */Block.__(1, [
                    "Seq",
                    of_regex(r[0]),
                    of_regex(r[1])
                  ]);
      case 3 : 
          return /* One */Block.__(0, [
                    "Star",
                    of_regex(r[0])
                  ]);
      
    }
  }
}

export {
  of_regex ,
  
}
/* CharSet-ReasonReNfa Not a pure module */
