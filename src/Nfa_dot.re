/* Representation of DOT graphs, and conversion from NFAs to DOT

   The DOT specification is here:
   https://graphviz.gitlab.io/_pages/doc/info/lang.html

   The code here supports a slightly restricted subset without
   subgraphs, multi-node edges, or "ports". */

module Digraph: {
  type t;

  module Node: {
    type t;
    let make: (~id: string) => t;
    let with_attrs: (t, list((string, string))) => t;
  };

  let empty: t;
  let with_name: (t, string) => t;
  let with_node: (t, Node.t) => t;
  let with_edge:
    (t, ~attrs: list((string, string))=?, (Node.t, Node.t)) => t;
  let with_attrs: (t, list((string, string))) => t;
  let format: (Format.formatter, t) => unit;
} = {
  type id = string;
  type attr = (id, id);

  let format_attrs = formatter =>
    fun
    | [] => ()
    | attrs => {
        Format.fprintf(formatter, "[@ @[");
        List.iter(
          ((k, v)) => Format.fprintf(formatter, "%S@ =@ %S;", k, v),
          attrs,
        );
        Format.fprintf(formatter, "]@]");
      };

  module Node = {
    type t = (id, list(attr));

    let make = (~id) => (id, []);
    let with_attrs = ((id, attrs), attrs') => (id, attrs @ attrs');
    let format = (formatter, (id, attrs)) =>
      Format.fprintf(formatter, "%a@ %S", format_attrs, attrs, id);
    let id = ((id, _)) => id;
  };
  type stmt =
    | Node(Node.t)
    | Edge(Node.t, Node.t, list(attr))
    | Attr(id, id);
  type t = (option(id), list(stmt));

  let empty = (None, []);
  let with_attrs = ((id, stmts), attrs) => (
    id,
    stmts @ List.map(((k, v)) => Attr(k, v), attrs),
  );
  let with_node = ((id, stmts), node) => (id, stmts @ [Node(node)]);
  let with_edge = ((id, stmts), ~attrs=?, (n1, n2)) =>
    switch (attrs) {
    | None => (id, stmts @ [Edge(n1, n2, [])])
    | Some(attrs) => (id, stmts @ [Edge(n1, n2, attrs)])
    };
  let with_name = ((_, s), n) => (Some(n), s);

  let format_stmt = formatter =>
    fun
    | Node(node) =>
      Format.fprintf(formatter, "node@ @[%a@]", Node.format, node)
    | Edge(n1, n2, attrs) =>
      Format.fprintf(
        formatter,
        "@[@[%S@ ->@ %S@]@ %a@]",
        Node.id(n1),
        Node.id(n2),
        format_attrs,
        attrs,
      )
    | Attr(k, v) => Format.fprintf(formatter, "@[%S@ =@ %S@];", k, v);

  let format = (formatter, (id, stmts)) => {
    let pr = fmt => Format.fprintf(formatter, fmt);
    switch (id) {
    | None => pr("@[digraph {@\n")
    | Some(id) => pr("@]digraph %S{@[", id)
    };
    List.iter(pr("@ @ @[%a@]@\n", format_stmt), stmts);
    pr("}@]");
  };
};

type digraph = Digraph.t;
let format_digraph = Digraph.format;

module CharSet = Set.Make(Char);

let edge_name = s =>
  switch (CharSet.cardinal(s)) {
  | 0 => "{}"
  | 1 => String.make(1, CharSet.choose(s))
  | 256 => "."
  | _ =>
    "{"
    ++ String.concat(" ", List.map(String.make(1), CharSet.elements(s)))
    ++ "}"
  };

let digraph_of_nfa: Nfa.nfa => Digraph.t =
  nfa => {
    let states = Hashtbl.create(10);
    let edges = Hashtbl.create(10);
    let make_node = {
      let counter = ref(0);
      n => {
        let name = string_of_int(counter^);
        incr(counter);
        let node = Digraph.Node.make(~id=name);
        let shape =
          if (Nfa.StateSet.S.mem(n, nfa.Nfa.finals)) {
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

    let rec step = state =>
      /* Accumulate nodes and edges, using the states/edges tables as
         'seen lists' to ensure each node and edge is only visited once */
      if (!Hashtbl.mem(states, state)) {
        Hashtbl.add(states, state, make_node(state));
        Nfa.CharMapStateSet.M.iter(
          (c, targets) =>
            Nfa.StateSet.S.iter(
              target => {
                add_edge(state, c, target);
                step(target);
              },
              targets,
            ),
          nfa.Nfa.next(state),
        );
      };

    step(nfa.start);
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

    /*** Add the initial edge */
    let dg =
      Digraph.with_edge(dg, (input, Hashtbl.find(states, nfa.start)));
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
