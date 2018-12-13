let analyze = (regexp) => {
  let parsed = RegexParser.parse(regexp);
  let compiled = Glushkov.compile(parsed);
  let dfa = RabinScott.determinize(compiled.nfa);

  /** let minimize = dfa => reverse(dfa) -> RabinScott.determinize -> reverse -> RabinScott.determinize; */
  let reversed = Brzozowski.reverse(dfa);
  let dfa2 = RabinScott.determinize(reversed);
  let reversed2 = Brzozowski.reverse(dfa2);
  let dfa_minimal = RabinScott.determinize(reversed2);

  (
    Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(compiled.nfa)),
    RegexParseTree.of_regex(parsed),
    compiled.nullable,
    compiled.firsts,
    compiled.lasts,
    compiled.factors,
    compiled.annotated,
    compiled.transitions,
    Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(Brzozowski.inject(dfa))),
    Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(reversed)),
    Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(Brzozowski.inject(dfa2))),
    Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(reversed2)),
    Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(Brzozowski.inject(dfa_minimal))),

  );
};
