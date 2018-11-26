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
