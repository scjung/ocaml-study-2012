type 'a node = { id : int; mutable value : 'a }

type 'a graph = {
  mutable nodes : 'a node list;
  mutable edges : (int * int) list
}

let fresh_id = let id = ref 0 in fun () -> incr id; !id

let create () = { nodes = []; edges = [] }

let node x = { id = fresh_id (); value = x }

let edge n1 n2 : int * int = if n1 < n2 then (n1, n2) else (n2, n1)

let add_node g n = g.nodes <- n :: g.nodes

let mem_node g id = List.exists (fun n -> n.id = id) g.nodes

let find_node g id = List.find (fun n -> n.id = id) g.nodes

let mem_edge g n1 n2 = List.mem (edge n1 n2) g.edges

let add_edge g n1 n2 =
  if mem_node g n1.id && mem_node g n2.id then
    g.edges <- (edge n1.id n2.id) :: g.edges
  else
    invalid_arg "add_edge"

let delete_edge g n1 n2 =
  g.edges <- List.filter ((<>) (edge n1 n2)) g.edges

let get_node_value g id = (find_node g id).value

let set_node_value g id x = (find_node g id).value <- x
