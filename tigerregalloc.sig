signature tigerregalloc = 
sig
	type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table
	
	val spill : ((tigertemp.temp) tigerset.set) * tigerframe.frame * tigerassem.instr list -> (tigerassem.instr list * (tigertemp.temp) tigerset.set) 
end
