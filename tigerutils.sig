signature tigerutils = 
sig

val canonize: tigertree.stm -> tigertree.stm list

val divideFrags: (tigerframe.strfrag list * tigerframe.procfrag list)  -> tigerframe.frag list 
					-> (tigerframe.strfrag list * tigerframe.procfrag list) 

val genCanonFmts: tigerframe.procfrag list -> tigerframe.cproc list

val geninstr: tigerframe.cproc list ->  tigerframe.iproc list

val printFragments: tigerframe.frag list -> unit

val printCanonFmts: (tigerframe.strfrag list * tigerframe.cproc list) -> unit

val printCode: tigerframe.iproc list -> unit

val genFinal : tigercolor.allocation -> tigerassem.instr list -> string

val sameMove : tigercolor.allocation ->  tigerassem.instr -> bool

end

