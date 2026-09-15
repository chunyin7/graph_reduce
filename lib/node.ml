type node_type = A | B

type port_type = Principal | Auxilary of int (* id of aux port *)

type node_id = int

type port = {
  node: node_id;
  kind: port_type;
}

type node = {
  id: node_id;
  kind: node_type;
  arity: int;
}
