signature tigerframe =
sig

type frame
type register = string
val rv : tigertemp.temp
val ov : tigertemp.temp
val fp : tigertemp.temp
val sp : tigertemp.temp		(* stack pointer *)
val lr : tigertemp.temp		(* link register *) 
val pc : tigertemp.temp		(* program counter *)
datatype access = InFrame of int | InReg of tigertemp.temp
val fpPrev : int
val fpPrevLev : int
val newFrame : {name: tigertemp.label, formals: bool list} -> frame
val name : frame -> tigertemp.label
val string : tigertemp.label * string -> string
val formals : frame -> access list
val allocArg : frame -> bool -> access
val allocLocal : frame -> bool -> access
val maxRegFrame : frame -> int
val wSz : int
val log2WSz : int
val specialregs : tigertemp.temp list
val argregs : tigertemp.temp list
val calldefs : tigertemp.temp list
val callersaves : tigertemp.temp list
val calleesaves : tigertemp.temp list
val usable : tigertemp.temp list
val exp : access -> tigertree.exp -> tigertree.exp
val externalCall : string * tigertree.exp list -> tigertree.exp
val procEntryExit1 : frame * tigertree.stm -> tigertree.stm
val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list
val procEntryExit3 : frame * tigerassem.instr list -> { prolog : string, body: tigerassem.instr list, epilog : string }
type strfrag = tigertemp.label * string
type procfrag = {body: tigertree.stm, frame: frame}
type cproc  = {body: tigertree.stm list, frame: frame} 
type iproc = tigerassem.instr list * frame
datatype frag = PROC of procfrag | STRING of strfrag 
val genstring: strfrag -> string
val head_foot: string * string * string * bool -> string

end
