signature tigerflow = 
sig
   datatype flowgraph =
   
    FGRAPH of { control : tigergraph.graph, 
                 def :  (tigertemp.temp list) tigergraph.table,
                 use :  (tigertemp.temp list) tigergraph.table,
                 ismove : bool tigergraph.table }
    
   val instrs2graph : tigerassem.instr list ->
                          flowgraph * tigergraph.node list
                          
   val debugflowprint : (flowgraph * tigergraph.node list * tigerassem.instr list) -> unit                       
   
end

