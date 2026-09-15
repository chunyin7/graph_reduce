open Node

type graph = {
  nodes: node list;
  edges: (port * port) list;
}
