signature tigercolor = 
sig

    type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table


(*
	val color : { interference : tigerliveness.igraph,
				  initial: allocation, 
				  spillCost: tigergraph.node -> int, 
				  registers: tigerframe.register list} 
					-> allocation * tigertemp.temp list
*)              
    val main : (tigerassem.instr list * tigerframe.frame) -> (tigerassem.instr list * allocation)
end
