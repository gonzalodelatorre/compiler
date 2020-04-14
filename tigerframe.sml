(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int

val fp = "fp"				(* Frame pointer *)
val rv = "r0"				(* Return value  *) 
val sp = "sp"				(* Stack pointer *)
val lr = "lr"				(* Link register *) 
val pc = "pc"  				(* Program counter *)
val ov = "OV"				(* Overflow value (edx en el 386) *)
val calldefs = [rv, "r1", "r2", "r3"]

val specialregs = [fp, sp, lr, pc]

(* P208. Registers in which to pass outgoing arguments - including the static linl *)
val argregs = [rv, "r1", "r2", "r3"]

(* P208. Registers that the callee may trash *)
val callersaves = ["r0","r1","r2","r3"]

(* P208. Registers that the called procedure - callee - must preserve unchanged - or save and restore - *)
val calleesaves = ["r4","r5","r6","r7","r8","r9","r10", fp] 

(*
General Purpose
*)
val usable = ["r0","r1","r2","r3","r4","r5","r6","r7","r8","r9","r10"] 



(*
"Chapter 6"
First 16 registers are general purpose and used to hold data or addresses that will be used with arithmetic or
logical operations. Of these, the first 13 are generic and denoted r0 thru r12

The registers r13 and r14 – though technically available to use as you wish – are understood to have special
functions and will be used by the compiler for special functions 

Register r13 is reserved for a stack pointer. Register r14 is the “link” register that is
used to hold return addresses for function calls. Register r15 is the program counter regardless
of whether you are writing in assembler or C. It will continually be updating automatically from
hardware and really cannot be used for anything else. 


*)


val backup = calleesaves@[lr]  (* backup y restore deben estar alineados *)
val restore = calleesaves@[pc]
val wSz = 4				(* Word size in bytes *)
val log2WSz = 2				(* Base 2 logarithm of word size in bytes *)
val fpPrev = 0				(* Offset (bytes) *)

val backupGap = wSz * (List.length backup)
val argsInicial = 0			(* Quantity *)
val argsGap = wSz			(* Bytes *)
val argsOffInicial = backupGap + wSz    (* Words *)
val regInicial = 1			(* Reg *)
val localsInicial = 0			(* Words *)
val numLocalsInicial = 0    		(* Sreg *)
val fpPrevLev = localsInicial		(* Offset (bytes) *)
val localsGap = wSz 			(* Bytes *)

datatype access = InFrame of int | InReg of tigertemp.label

type frame = {
	name : string,
	formals : bool list,
	locals : bool list,
	actualArg : int ref,
	actualLocal : int ref,
	actualReg : int ref, 
        localsInFrame : int ref, 
	argsAcc : (access list) ref
}

type register = string
type strfrag = tigertemp.label * string
type procfrag = {body: tigertree.stm, frame: frame}
type cproc  = {body: tigertree.stm list, frame: frame} 
type iproc = tigerassem.instr list * frame

datatype frag = PROC of procfrag | STRING of strfrag 



(*

allocArg

*)
fun allocArg (f: frame) b = 
  let
     val acc = case b of 
	  true => InFrame (!(#actualArg f)*wSz + argsOffInicial)
	| false => InReg (tigertemp.newtemp())
  in 
	#actualArg f := !(#actualArg f)+1;
        (* XXX Debug *)
	print("ENTRY tigerframe.allocArg(...):\n");
        print("tigerframe.allocArg(...): frame: "^ #name f  ^ " actualArg: " ^ Int.toString(!(#actualArg f)) ^"\n");
        print("END tigerframe.allocArg\n");
	acc
  end

(*

allocLocal

*)
fun allocLocal (f: frame) b = 
    case b of
        true =>
            let	
	        val ret = InFrame(!(#actualLocal f))
            in	
                #localsInFrame f := !(#localsInFrame f) + 1;
                #actualLocal f := (!(#actualLocal f) - wSz); 
                (* XXX Debug *)
	        print("ENTRY tigerframe.allocLocal(...):\n");
                print("tigerframe.allocLocal(...): frame: "^ #name f  ^ " localsInFrame: " ^ Int.toString(!(#localsInFrame f)) ^" actualLocal: "^Int.toString(!(#actualLocal f))^"\n");
		print("END tigerframe.allocLocal\n");
                ret
            end 
        | false => InReg(tigertemp.newtemp())

(*

newFrame

*)
fun newFrame{name, formals} = 
 let
	val newframe = {	
			name=name,
			formals = formals,
			locals = [],
			actualArg = ref argsInicial,
			actualLocal = ref localsInicial,
			actualReg = ref regInicial,
                        localsInFrame = ref numLocalsInicial,
			argsAcc = ref ([] : (access list)) 
			}
            
            
	 fun allocFormal n = if n < (List.length argregs) 
		             then allocLocal newframe (List.nth (formals, n))
		             else allocArg newframe (List.nth (formals, n))
	 
 in

	(* XXX Debug *)
	print("ENTRY tigerframe.newFrame(...):\n");
	print("tigerframe.newFrame(...): frame name: "^ name ^ " formals length: " ^ Int.toString(List.length formals)^"\n");
        print("END tigerframe.newFrame(...):\n");

	(* tabulate (n, f) = [f(0), f(1), ..., f(n-1)] *)
	#argsAcc newframe := List.tabulate (List.length formals, allocFormal);
	newframe
 end

(*

name

*)
fun name(f: frame) = #name f

(*

string

*)
fun string(l, s) = l^tigertemp.makeString(s)^"\n"

(*

formals

*)
fun formals({argsAcc, ...}: frame) = !argsAcc

(*

maxRegFrame

*)
fun maxRegFrame(f: frame) = !(#actualReg f)
  
(*

exp

*)
fun exp(InFrame k) e = (if (k >= 0) then MEM(BINOP(PLUS, e, CONST k)) else MEM(BINOP(MINUS, e, CONST (~k))))
	| exp(InReg l) _ = TEMP l
	
(*

externalCall

*)	
fun externalCall(s, l) = CALL(NAME s, l)

(*

seq

*)
fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

(*

mkpushlist

*)
fun mkpushlist [] = "{}"
  | mkpushlist (x::xs) = 
    let
        fun mkpushlist1 [] = "" 
           |mkpushlist1 (t::ts) = ","^t^(mkpushlist1 ts)
    in 
        "{"^x^(mkpushlist1 xs)^"}" 
    end

(*

procEntryExit1

Pág 261

Instrucciones para mover de los argumentos de la función a los locals donde la función ve internamente.

*)
fun procEntryExit1 (fr:frame, body) =  
let 
	val argsAcc = #argsAcc fr

        (*

        *)
	fun aux [] _ = []
	  | aux (acc::accs) n = if n < List.length argregs 
			        then tigertree.MOVE(exp acc (TEMP fp), TEMP (List.nth(argregs, n))) :: aux accs (n+1) 
			        else if not (List.nth ((#formals fr), n)) 
				     then (* Si no escapa la copiamos a un temp *) 
			       		let 
				 	   val varoffset = (n - List.length argregs)*wSz + argsOffInicial
                                           (* TODO Debug *)        
					   val _ = print("tigreframe.procEntryExit1(...) varoffset:  " ^ Int.toString(varoffset)^ "\n")
				           val src = (if (varoffset >= 0) 
                                            	      then MEM(BINOP(PLUS, CONST varoffset, TEMP fp)) 
				                      else MEM(BINOP(MINUS, CONST (~varoffset), TEMP fp))) 
			                in
				          tigertree.MOVE(exp acc (TEMP fp), src) :: aux accs (n+1)
			                end
			             else aux accs (n+1) (* Si escapa y no esta en registro, no hacemos nada *) 

	val moveargs = aux (!argsAcc) 0 
										
	
in 
        (* TODO PRINT *)
	seq(moveargs @ [body]) 
end

(*

procEntryExit2 

Appends a "sink" instruction to the function body to tell the register allocator that certain
registers are live at procedure exit. Pág 208 

*)
fun procEntryExit2 (frame, body) = body @ [tigerassem.OPER{assem="", src=[rv], dst=[], jump=SOME[]}] 

(*

procEntryExit3

Pág 261

Creates the procedure prologue and epilogue assembly language

*)
fun procEntryExit3 (frame:frame, instrs) = {

				          prolog = 
   					       "\n\t@prologo:\n"^
                                               ".align 2\n"^ 
                                               ".global " ^ #name frame ^ "\n" ^
                                               ".type   " ^ #name frame ^", %function\n" ^
                                               "\t" ^ #name frame ^ ":\n" ^  
                                               "\tpush "^mkpushlist backup^"\n"^
					       "\tsub     fp, sp, #4\n" ^ 
                                               "\tsub     sp, #"^(Int.toString (!(#localsInFrame frame) * wSz ))^"\n\n",
                                          body = instrs,
                                          epilog = 
						"@epilogo\n"^ 
						"\tadd     sp, #"^(Int.toString (!(#localsInFrame frame) * wSz))^"\n"^
                                                "\tpop "^mkpushlist restore^"\n"
                                            
                                             }


(*

genstring

Pág 261.
String containing the assembly-language intructions required to define and initialize a string literal.

*)
fun genstring (lab, str) = "\t.align\t2\n"^lab^":\n"^
			   "\t.long\t"^(Int.toString (String.size str))^"\n"^
			   "\t.ascii\t\""^(String.toCString str)^"\"\n"
							
(*

head_foot

*)
fun head_foot (rodata, text, progname, archbanana) = 
 
         "\t.arch armv7-a\n"^
	 "\t.file\t\""^progname^"\"\n"^ (* Source file name *)
	 "\t.global\t__aeabi_idiv\n"^   (* Modulo división *)
	 "\t.section\t.rodata\n"^       (* Section ".rodata" - Read Only *)
	  rodata^
	 "\t.text\n"^                   (* ".text" section, where code goes. *)     
	  text
 	


end

