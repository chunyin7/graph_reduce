open Node

type graph = { nodes : node list; edges : (port * port) list }

let find_node id nodes = List.find (fun node -> node.id = id) nodes
let rule_a_b graph a b = graph

let step graph =
  match
    List.find_opt
      (fun ((p1, p2) : port * port) ->
        p1.kind = Principal && p2.kind = Principal)
      graph.edges
  with
  | Some (p1, p2) -> (
      let n1 = find_node p1.node graph.nodes in
      let n2 = find_node p2.node graph.nodes in
      match (n1.kind, n2.kind) with
      | A, B -> rule_a_b graph n1.id n2.id
      | _ -> graph)
  | None -> graph
