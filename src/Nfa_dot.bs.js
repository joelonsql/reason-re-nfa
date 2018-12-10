// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE

import * as Block from "../node_modules/bs-platform/lib/es6/block.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Format from "../node_modules/bs-platform/lib/es6/format.js";
import * as Hashtbl from "../node_modules/bs-platform/lib/es6/hashtbl.js";
import * as CharSet$ReasonReNfa from "./CharSet.bs.js";
import * as Digraph$ReasonReNfa from "./Digraph.bs.js";
import * as Glushkov$ReasonReNfa from "./Glushkov.bs.js";
import * as StateSet$ReasonReNfa from "./StateSet.bs.js";
import * as Caml_builtin_exceptions from "../node_modules/bs-platform/lib/es6/caml_builtin_exceptions.js";
import * as RegexParser$ReasonReNfa from "./RegexParser.bs.js";
import * as CharMapStateSet$ReasonReNfa from "./CharMapStateSet.bs.js";

function digraph_of_nfa(nfa) {
  var states = Hashtbl.create(undefined, 10);
  var edges = Hashtbl.create(undefined, 10);
  var counter = /* record */[/* contents */0];
  var make_node = function (n) {
    var name = String(counter[0]);
    counter[0] = counter[0] + 1 | 0;
    var node = Digraph$ReasonReNfa.Node[/* make */0](name);
    var shape = Curry._2(StateSet$ReasonReNfa.mem, n, nfa[/* finals */1]) ? "doublecircle" : "circle";
    return Digraph$ReasonReNfa.Node[/* with_attrs */1](node, /* :: */[
                /* tuple */[
                  "shape",
                  shape
                ],
                /* [] */0
              ]);
  };
  var add_edge = function (source, c, target) {
    var tmp;
    var exit = 0;
    var set;
    try {
      set = Hashtbl.find(edges, /* tuple */[
            source,
            target
          ]);
      exit = 1;
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        tmp = Curry._1(CharSet$ReasonReNfa.singleton, c);
      } else {
        throw exn;
      }
    }
    if (exit === 1) {
      tmp = Curry._2(CharSet$ReasonReNfa.add, c, set);
    }
    return Hashtbl.replace(edges, /* tuple */[
                source,
                target
              ], tmp);
  };
  var step = function (state) {
    if (Hashtbl.mem(states, state)) {
      return 0;
    } else {
      Hashtbl.add(states, state, make_node(state));
      return Curry._2(CharMapStateSet$ReasonReNfa.iter, (function (c, targets) {
                    return Curry._2(StateSet$ReasonReNfa.iter, (function (target) {
                                  add_edge(state, c, target);
                                  return step(target);
                                }), targets);
                  }), Curry._1(nfa[/* next */2], state));
    }
  };
  Curry._2(StateSet$ReasonReNfa.iter, step, nfa[/* start */0]);
  var input = Digraph$ReasonReNfa.Node[/* with_attrs */1](Digraph$ReasonReNfa.Node[/* make */0](""), /* :: */[
        /* tuple */[
          "shape",
          "none"
        ],
        /* :: */[
          /* tuple */[
            "width",
            "0"
          ],
          /* [] */0
        ]
      ]);
  var dg = Digraph$ReasonReNfa.with_node(Digraph$ReasonReNfa.with_attrs(Digraph$ReasonReNfa.empty, /* :: */[
            /* tuple */[
              "rankdir",
              "LR"
            ],
            /* [] */0
          ]), input);
  var dg$1 = Hashtbl.fold((function (param, node, dg) {
          return Digraph$ReasonReNfa.with_node(dg, node);
        }), states, dg);
  var dg$2 = Curry._3(StateSet$ReasonReNfa.fold, (function (s, dg) {
          return Digraph$ReasonReNfa.with_edge(dg, undefined, /* tuple */[
                      input,
                      Hashtbl.find(states, s)
                    ]);
        }), nfa[/* start */0], dg$1);
  return Hashtbl.fold((function (param, s, dg) {
                return Digraph$ReasonReNfa.with_edge(dg, /* :: */[
                            /* tuple */[
                              "label",
                              CharSet$ReasonReNfa.to_string(s)
                            ],
                            /* [] */0
                          ], /* tuple */[
                            Hashtbl.find(states, param[0]),
                            Hashtbl.find(states, param[1])
                          ]);
              }), edges, dg$2);
}

function test(param) {
  var parsed = RegexParser$ReasonReNfa.parse("a(b|c)d");
  var compiled = Glushkov$ReasonReNfa.compile(parsed);
  var dot = Curry._2(Format.asprintf(/* Format */[
            /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                    /* Flush_newline */4,
                    /* End_of_format */0
                  ])]),
            "%a@."
          ]), Digraph$ReasonReNfa.format, digraph_of_nfa(compiled[/* nfa */0]));
  if (dot === "digraph {\n  \"rankdir\" = \"LR\";\n  node [ \"shape\" = \"none\";\"width\" = \"0\";] \"\"\n  node [ \"shape\" = \"circle\";] \"2\"\n  node [ \"shape\" = \"doublecircle\";] \"3\"\n  node [ \"shape\" = \"circle\";] \"0\"\n  node [ \"shape\" = \"circle\";] \"1\"\n  \"\" -> \"0\" \n  \"0\" -> \"1\" [ \"label\" = \"a\";]\n  \"1\" -> \"2\" [ \"label\" = \"[bc]\";]\n  \"2\" -> \"3\" [ \"label\" = \"d\";]\n}\n") {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "Nfa_dot.re",
            105,
            2
          ]
        ];
  }
}

var format_digraph = Digraph$ReasonReNfa.format;

var edge_name = CharSet$ReasonReNfa.to_string;

export {
  format_digraph ,
  edge_name ,
  digraph_of_nfa ,
  test ,
  
}
/* Format Not a pure module */
