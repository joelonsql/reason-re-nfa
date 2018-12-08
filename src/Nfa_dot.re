/* Representation of DOT graphs, and conversion from NFAs to DOT

   The DOT specification is here:
   https://graphviz.gitlab.io/_pages/doc/info/lang.html

   The code here supports a slightly restricted subset without
   subgraphs, multi-node edges, or "ports". */

type digraph = Digraph.t;
let format_digraph = Digraph.format;

let edge_name = CharSet.to_string;

let digraph_of_nfa: Nfa.nfa => Digraph.t = (nfa) => {
  let states = Hashtbl.create(10);
  let edges = Hashtbl.create(10);
  let make_node = {
    let counter = ref(0);
    n => {
      let name = string_of_int(counter^);
      incr(counter);
      let node = Digraph.Node.make(~id=name);
      let shape =
        if (StateSet.mem(n, nfa.Nfa.finals)) {
          "doublecircle";
        } else {
          "circle";
        };
      Digraph.Node.with_attrs(node, [("shape", shape)]);
    };
  };

  let add_edge = (source, c, target) =>
    Hashtbl.replace(edges, (source, target)) @@
    (
      switch (Hashtbl.find(edges, (source, target))) {
      | exception Not_found => CharSet.singleton(c)
      | set => CharSet.add(c, set)
      }
    );

  let rec step = (state) =>
    /* Accumulate nodes and edges, using the states/edges tables as
        'seen lists' to ensure each node and edge is only visited once */
    if (!Hashtbl.mem(states, state)) {
      Hashtbl.add(states, state, make_node(state));
      CharMapStateSet.iter(
        (c, targets) =>
          StateSet.iter(
            target => {
              add_edge(state, c, target);
              step(target);
            },
            targets,
          ),
        nfa.Nfa.next(state),
      );
    };

  StateSet.iter(step, nfa.start);
  /*** Empty node to the left of the start state */
  let input =
    Digraph.Node.with_attrs(
      Digraph.Node.make(~id=""),
      [("shape", "none"), ("width", "0")],
    );
  /*** Initial empty digraph */
  let dg =
    Digraph.with_node(
      Digraph.with_attrs(Digraph.empty, [("rankdir", "LR")]),
      input,
    );
  /*** Add the state nodes */
  let dg =
    Hashtbl.fold(
      (_, node, dg) => Digraph.with_node(dg, node),
      states,
      dg,
    );

  /*** Add the initial edges */
  let dg =
    StateSet.fold(
      (s, dg) => Digraph.with_edge(dg, (input, Hashtbl.find(states, s))),
      nfa.start,
      dg
    );
  /*** Add the other edges */
  Hashtbl.fold(
    ((source, target), s, dg) =>
      Digraph.with_edge(
        dg,
        ~attrs=[("label", edge_name(s))],
        (Hashtbl.find(states, source), Hashtbl.find(states, target)),
      ),
    edges,
    dg,
  );
};

let test = () => {
  let parsed = RegexParser.parse("a(b|c)d");
  let compiled = Glushkov.compile(parsed);
  let dot = Format.asprintf("%a@.", format_digraph, digraph_of_nfa(compiled.nfa));
  assert(dot == {|digraph {
  "rankdir" = "LR";
  node [ "shape" = "none";"width" = "0";] ""
  node [ "shape" = "circle";] "2"
  node [ "shape" = "doublecircle";] "3"
  node [ "shape" = "circle";] "0"
  node [ "shape" = "circle";] "1"
  "" -> "0" 
  "0" -> "1" [ "label" = "a";]
  "1" -> "2" [ "label" = "[bc]";]
  "2" -> "3" [ "label" = "d";]
}
|});
}