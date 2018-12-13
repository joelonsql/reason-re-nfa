// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE

import * as Block from "../node_modules/bs-platform/lib/es6/block.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as Format from "../node_modules/bs-platform/lib/es6/format.js";
import * as Nfa_dot$ReasonReNfa from "./Nfa_dot.bs.js";
import * as Glushkov$ReasonReNfa from "./Glushkov.bs.js";
import * as Brzozowski$ReasonReNfa from "./Brzozowski.bs.js";
import * as RabinScott$ReasonReNfa from "./RabinScott.bs.js";
import * as RegexParser$ReasonReNfa from "./RegexParser.bs.js";
import * as RegexParseTree$ReasonReNfa from "./RegexParseTree.bs.js";

function analyze(regexp) {
  var parsed = RegexParser$ReasonReNfa.parse(regexp);
  var compiled = Glushkov$ReasonReNfa.compile(parsed);
  var dfa = RabinScott$ReasonReNfa.determinize(compiled[/* nfa */0]);
  var reversed = Brzozowski$ReasonReNfa.reverse(dfa);
  var dfa2 = RabinScott$ReasonReNfa.determinize(reversed);
  var reversed2 = Brzozowski$ReasonReNfa.reverse(dfa2);
  var dfa_minimal = RabinScott$ReasonReNfa.determinize(reversed2);
  return /* tuple */[
          Curry._2(Format.asprintf(/* Format */[
                    /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                            /* Flush_newline */4,
                            /* End_of_format */0
                          ])]),
                    "%a@."
                  ]), Nfa_dot$ReasonReNfa.format_digraph, Nfa_dot$ReasonReNfa.digraph_of_nfa(compiled[/* nfa */0])),
          RegexParseTree$ReasonReNfa.of_regex(parsed),
          compiled[/* nullable */2],
          compiled[/* firsts */3],
          compiled[/* lasts */4],
          compiled[/* factors */5],
          compiled[/* annotated */1],
          compiled[/* transitions */6],
          Curry._2(Format.asprintf(/* Format */[
                    /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                            /* Flush_newline */4,
                            /* End_of_format */0
                          ])]),
                    "%a@."
                  ]), Nfa_dot$ReasonReNfa.format_digraph, Nfa_dot$ReasonReNfa.digraph_of_nfa(Brzozowski$ReasonReNfa.inject(dfa))),
          Curry._2(Format.asprintf(/* Format */[
                    /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                            /* Flush_newline */4,
                            /* End_of_format */0
                          ])]),
                    "%a@."
                  ]), Nfa_dot$ReasonReNfa.format_digraph, Nfa_dot$ReasonReNfa.digraph_of_nfa(reversed)),
          Curry._2(Format.asprintf(/* Format */[
                    /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                            /* Flush_newline */4,
                            /* End_of_format */0
                          ])]),
                    "%a@."
                  ]), Nfa_dot$ReasonReNfa.format_digraph, Nfa_dot$ReasonReNfa.digraph_of_nfa(Brzozowski$ReasonReNfa.inject(dfa2))),
          Curry._2(Format.asprintf(/* Format */[
                    /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                            /* Flush_newline */4,
                            /* End_of_format */0
                          ])]),
                    "%a@."
                  ]), Nfa_dot$ReasonReNfa.format_digraph, Nfa_dot$ReasonReNfa.digraph_of_nfa(reversed2)),
          Curry._2(Format.asprintf(/* Format */[
                    /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                            /* Flush_newline */4,
                            /* End_of_format */0
                          ])]),
                    "%a@."
                  ]), Nfa_dot$ReasonReNfa.format_digraph, Nfa_dot$ReasonReNfa.digraph_of_nfa(Brzozowski$ReasonReNfa.inject(dfa_minimal)))
        ];
}

export {
  analyze ,
  
}
/* Format Not a pure module */
