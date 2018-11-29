let analyze = (regexp) => {
  let parsed = RegexParser.parse(regexp);
  let compiled = Glushkov.compile(parsed);
  (
    Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(compiled.nfa)),
    RegexParseTree.of_regex(parsed),
    compiled.nullable,
    compiled.firsts,
    compiled.lasts,
    compiled.factors,
    compiled.annotated,
    compiled.transitions
  );
};
