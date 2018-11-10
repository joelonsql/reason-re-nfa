type digraph;

let format_digraph: (Format.formatter, digraph) => unit;

let digraph_of_nfa: Nfa.nfa => digraph;
