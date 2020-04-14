signature tigerliveness = 
sig
	datatype igraph = 
		IGRAPH of {graph: tigergraph.graph,
					tnode: tigertemp.temp -> tigergraph.node, 
					gtemp: tigergraph.node -> tigertemp.temp, 
					moves: (tigergraph.node * tigergraph.node) list
				}
				
	type liveness = (tigergraph.node -> tigertemp.temp list) 
	
	(* P224 *)			
 	val interferenceGraph: tigerflow.flowgraph -> igraph * liveness
	
	(* debug *)
        val printLiveOut : tigerflow.flowgraph -> unit 		
	val show : igraph -> unit
end
