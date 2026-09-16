open Node

type graph = { nodes : node list; edges : (port * port) list }

let find_node id nodes = List.find (fun node -> node.id = id) nodes

let aux_id (port : port) =
  match port.kind with Auxilary id -> Some id | Principal -> None

let unwrap_aux_id (port : port) =
  match port.kind with
  | Auxilary id -> id
  | Principal -> failwith "Expected auxilary port."

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

(* remove the nodes connect their auxilaries *)
let rule_a_b graph a b =
  let new_edges =
    List.map
      (fun id ->
        ({ node = a; kind = Auxilary id }, { node = b; kind = Auxilary id }))
      (0 -- 1)
  in
  let edges' =
    List.filter
      (fun ((p1, p2) : port * port) ->
        p1.node != a && p2.node != a && p1.node != b && p2.node != b)
      graph.edges
    |> List.append new_edges
  in
  {
    nodes = List.filter (fun n -> n.id != a && n.id != b) graph.nodes;
    edges = edges';
  }

let step graph =
  match
    List.find_opt
      (fun ((p1, p2) : port * port) ->
        p1.kind = Principal && p2.kind = Principal)
      graph.edges
  with
  | Some e -> (
      let p1, p2 = e in
      let n1 = find_node p1.node graph.nodes in
      let n2 = find_node p2.node graph.nodes in
      match (n1.kind, n2.kind) with
      | A, B -> rule_a_b graph n1.id n2.id
      | _ -> graph)
  | None -> graph
