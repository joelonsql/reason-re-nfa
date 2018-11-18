let analyze = (regexp) => {
    let parsed_regex = Regex.parse(regexp);
    let compiled_regex = Regex.compile(parsed_regex);
    (
        Format.asprintf("%a@.", Nfa_dot.format_digraph, Nfa_dot.digraph_of_nfa(compiled_regex)),
        Regex.regexp2parseTree(parsed_regex),
        compiled_regex.nullable,
        compiled_regex.firsts,
        compiled_regex.lasts,
        compiled_regex.factors,
        compiled_regex.annotated
    );
};
