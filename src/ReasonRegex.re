let regexp2dot = (regexp) => {
    let re = Regex.parse(regexp);
    let nfa = Regex.compile(re);
    let digraph = Nfa_dot.digraph_of_nfa(nfa);
    Format.asprintf("%a@.", Nfa_dot.format_digraph, digraph);
};

let regexp2parseTree = (regexp) => {
    Regex.regexp2parseTree(regexp);
};

let nullability = (regexp) => {
    let re = Regex.annotate(Regex.parse(regexp));
    Regex.l(re);
}

let firsts = (regexp) => {
    Regex.firsts(regexp);
}

let lasts = (regexp) => {
    Regex.lasts(regexp);
}

let letterpairs = (regexp) => {
    Regex.letterpairs(regexp);
}
