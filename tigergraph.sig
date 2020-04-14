signature tigergraph =
sig
    type graph
    type node
    
    val nodes: graph -> node list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list   (* succ+pred *)
    val eq: node*node -> bool
    val cmp: node*node -> order 

    val newGraph: unit -> graph
    val newNode : graph -> node
    exception GraphEdge
    val mk_edge: {from: node, to: node} -> unit
    val rm_edge: {from: node, to: node} -> unit

    (* Structure Splaymap XXX*)
    type 'a table = (node, 'a) Splaymap.dict
    val newTable : unit -> 'a table
 
    val nodename: node->string  (* debug only *)

    val printGraph : graph -> unit
    
    val printGraphWithNaming : graph -> (node -> string) -> unit
end
