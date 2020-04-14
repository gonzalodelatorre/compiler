structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero

(*
XXX Record
The Translate module handle the notion of nested scopes. Also useful for producing intermediate representation from abstract syntax.
*)
type level = {parent: frame option, frame: frame, level: int}

type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* XXX _tigermain: level = 0. *)

(*

getActualLev

*)
fun getActualLev() = !actualLevel

(*XXX Usado para inicializar la pila*)
val outermost: level = {parent=NONE,
			frame=newFrame{name="_tigermain", formals=[]}, 
			level=getActualLev()}


(*

newLevel

Used in tigerseman.sml, function transDec creates a new "nesting level" for each function by calling this function.
Returns a new frame. tigerseman keeps this level in its FuncEntry data structure for the function, so when it
comes across a function call it can pass the called function's level back to tigertrans. Also needs the label of the function's
machine-code entry point.

*)	
fun newLevel{parent={parent, frame, level}, name, formals} = {parent=SOME frame,
					    		      frame=newFrame{name=name, formals=true::formals}, (* XXX Agrego el static link. P142 *)
						              level=level+1}

(*

allocArg

*)
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b

(*
XXX Pág 141

allocLocal : level -> bool -> access

Used in tigerseman.sml, when proccesses a local variable declaration at level lev, it calls this function to create the variable in this level.
Params:
 + level 
 + b Indicates if the variable escapes
*)
fun allocLocal{parent, frame, level} b = 
let
in
(* XXX DEBUG *)
print("tigertrans.allocLocal(...) \n");
tigerframe.allocLocal frame b
end

(* 

formals

formals no es usable para funciones externas (usa el tail)

*)
fun formals{parent, frame, level} = tl (tigerframe.formals frame) (* XXX Saco el static link *)

(* 
XXX Pág 142
EX Expression
NX No result
CX Conditional
*)
datatype exp =
	Ex of tigertree.exp
	| Nx of tigertree.stm
	| Cx of label * label -> tigertree.stm

(*

seq : stm list  -> stm

*)
fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

(*

unEx : exp -> tigertree.exp

*)
fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0)
	| unEx (Cx cf) =
	let
		val r = newtemp()
		val t = newlabel()
		val f = newlabel()
	in
		ESEQ(seq [MOVE(TEMP r, CONST 1),
			cf (t, f),
			LABEL f,
			MOVE(TEMP r, CONST 0),
			LABEL t],
			TEMP r)
	end

(*

unNx : exp -> tigertree.stm

*)
fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
	let
		val t = newlabel()
		val f = newlabel()
	in
		seq [cf(t,f),
			LABEL t,
			LABEL f]
	end
(*

unCx : exp -> ((label, label) -> tigertree.stm)

*)
fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) =
	(fn (_,f) => JUMP(NAME f, [f]))
	| unCx (Ex (CONST _)) =
	(fn (t,_) => JUMP(NAME t, [t]))
	| unCx (Ex e) =
	(fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

(*

frag list -> string

*)
fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
	


(* While y for necesitan la última etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
		SOME l => l
		| NONE => raise Fail "break incorrecto!"			
end


val datosGlobs = ref ([]: frag list)

(*

procEntryExit

Pág 169. Descriptor for the function containing:
frame: Frame descriptor
body: Result returned from procEntryExit1

*)
fun procEntryExit{level: level, body} =
    let	
	 val body' = PROC{frame= #frame level, body=unNx body}
    in
         datosGlobs:=(!datosGlobs@[body']) 
    end

(*

getResult

*)
fun getResult() = !datosGlobs

(*

stringLen

*)
fun stringLen s =
	let	
            fun aux[] = 0
	      | aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
	      | aux(_::t) = 1+aux(t)
	in	
            aux(explode s) 
        end

(*

stringExp

*)
fun stringExp(s: string) =
	let	
	     val l = newlabel()

	     val cs = case String.fromCString s of  
				 SOME str => str
				| NONE => ""
		
             val _ = datosGlobs:=(!datosGlobs @ [STRING(l, cs)])
	in	
	     Ex(NAME l) 
        end

(*

preFunctionDec

*)	
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)

(*

functionDec

*)
fun functionDec(e, l, proc) =
	let	val body =  if proc then unNx e else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in	Ex(CONST 0) end

(* 

postFunctionDec

*)	
fun postFunctionDec() =
	(popSalida(); actualLevel := !actualLevel-1)

(*

unitExp

*)
fun unitExp() = Ex (CONST 0)

(*

nilExp

*)
fun nilExp() = Ex (CONST 0)

(*

intExp

*)
fun intExp i = Ex (CONST i)

(*

genDiff : int -> tigertree.exp

*)
fun genDiff 0 = TEMP fp
    | genDiff x = MEM (BINOP (PLUS, CONST fpPrevLev, genDiff (x-1)))

(*

simpleVar : access * int -> exp

Turns a  frame.access into an exp

Params
 + acc
 + level level of the function in wich x is used
Returns exp

*)
fun simpleVar(acc, nivel) =

let
(*XXX DEBUG*)

in
print("ENTRY tigretrans.simpleVar(...) getActualLev():  " ^ Int.toString(getActualLev())^ "\n");
print("tigretrans.simpleVar(...) nivel de la variable:  " ^ Int.toString(nivel)^ "\n");
Ex (tigerframe.exp acc (genDiff (getActualLev() - nivel)))
end

(*

varDec

Declaración de variable

*)
fun varDec(acc) = 
let
(*XXX DEBUG*)

in
print("ENTRY tigretrans.varDec(...) getActualLev():  " ^ Int.toString(getActualLev())^ "\n");
simpleVar(acc, getActualLev())
end

(*

fieldVar

*)
fun fieldVar(var, field) = 
let
	val r = unEx var
	val i = CONST field
	val rr = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP rr, r),
				 MOVE(TEMP ri, i),
				 EXP(externalCall("_checkNil", [TEMP rr])) ] ,
				 MEM(BINOP(PLUS, TEMP rr,
			            BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end 

(*

subscriptVar

*)
fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP ra, a),
				 EXP(externalCall("_checkIndexArray", [TEMP ra, i]))], 
		     MEM(BINOP(PLUS, TEMP ra,
						case i of 
						 CONST k => CONST (k * tigerframe.wSz)
						 | _ => BINOP(LSHIFT, i, CONST(tigerframe.log2WSz ))))))
			           

end

(*
 
recordExp : (exp * int) list -> exp

Pág 164

*)
fun recordExp l =
let
	val ret = newtemp() 
	fun gentemps 0 = [] | gentemps n = newtemp()::gentemps(n-1)
	val s = length l	
	fun aux ((e,s), t) = (MOVE(TEMP t, unEx e), s, TEMP t) (* XXX ( MOVE, EX, TEMP) *)
	val lexps = map aux (ListPair.zip(l, gentemps s)) (* XXX [(MOVE(TEMP t, unEx e), s, TEMP t), ...]*)
	val lexps' = map #1 lexps (* XXX  MOVE(TEMP t, unEx e) *)
	val l' = Listsort.sort (fn ((_,m,_),(_,n,_)) => Int.compare(m,n)) lexps  (* Ordena *)
in
	Ex ( ESEQ (seq (lexps'@[EXP (externalCall("_allocRecord", 
							(CONST s)::(map #3 l'))),
							MOVE (TEMP ret, TEMP rv)]), 
				 TEMP ret))
end 

(*

arrayExp

*)
fun arrayExp{size, init} =
let
	val s = unEx size
	val i = unEx init
in
	Ex (externalCall("_initArray", [s, i]))
end

(*

callExp : tigertemp.label * bool * bool * level * exp list -> exp

Pág 166

*)
fun callExp (name, ext, isproc, lev:level, ls) = 
		let
                        val _ = print("ENTRY tigertrans.callExp(...)\n")
			val _ = print("   Nombre de la funcion: "^ name^"\n")
			val _ = print("   Es externa: "^ Bool.toString(ext)^"\n")
			val _ = print("   Es procedimiento: "^ Bool.toString(isproc)^"\n")
			val _ = print("   getActualLev(): " ^ Int.toString(getActualLev())^ "\n")
			val _ = print("   Nivel de la funcion(): " ^ Int.toString(#level lev)^ "\n")
			val _ = print("END tigertrans.callExp(...)\n")
		        (* Acceso a static link *)
			val slx = genDiff ((getActualLev() - (#level lev))+1) 

			(* Pasamos los argumentos en registros para respetar convenciones de C.. *)
			fun prepArgs [] (rt, re) = (rt, re)
			  | prepArgs (h::t) (rt, re) = 
					case h of 
					  Ex (CONST s) => (prepArgs t ((CONST s)::rt, re))
					| Ex (NAME n) => (prepArgs t ((NAME n)::rt, re))
					| _ =>  let 
								val tmp = TEMP(newtemp())
							in 
								prepArgs t (tmp::rt, (MOVE(tmp, unEx h))::re)
							end

			val (rt, re) = prepArgs ls ([], [])
			
			(* Concatena static link a los argumentos *)
			val rt' = if ext then rt else slx::rt
			
			(* En este caso de funciones externas, encapsulamos la implementacion en tigerframe *)
			val cexp = if ext then externalCall(name, rt') else CALL(NAME name, rt')
		in 
			if isproc then 
				Nx (seq (re@[EXP cexp]))
			else
				let 
					val tmp = TEMP(newtemp());
				in 
					Ex (ESEQ( seq (re@[EXP cexp, MOVE(tmp, TEMP rv)]), tmp))
				end
		end	

(*

letExp

*)
fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits, unEx body))


(*

breakExp

*)
fun breakExp() = 
let
	val s = topSalida()
in 
	Nx (JUMP(NAME s, [s]))
end
	
(*

seqExp

*)	
fun seqExp ([]:exp list) = Nx (EXP(CONST 0))	
	| seqExp (exps:exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val unexps = map unNx exps
					in Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

(* 

While and For

*)
fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

(*

whileExp

*)
fun whileExp {test: exp, body: exp, lev:level} =
let
	val cf = unCx test
	val expb = unNx body
	val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
	Nx (seq[LABEL l1,
		cf(l2,l3),
		LABEL l2,
		expb,
		JUMP(NAME l1, [l1]),
		LABEL l3])
end

(*

forExp

*)
fun forExp {lo, hi, var, body} =
let
	val expb = unNx body
	val expv = unEx var
	val explo = unEx lo
	val exphi = unEx hi
	val (l1, l2, l3, l4) = (newlabel(), newlabel(), newlabel(), topSalida())
	val hit = newtemp()
in 
	Nx (seq[ MOVE(TEMP hit, exphi),  
			MOVE(expv, explo),
			JUMP(NAME l1, [l1]),
			LABEL l3,
			MOVE(expv, BINOP (PLUS, expv, CONST 1)),
			LABEL l1,
			CJUMP(LE, expv, TEMP hit, l2, l4),
			LABEL l2, 
			expb, 
			CJUMP(EQ, expv, TEMP hit, l4, l3), 
			LABEL l4])
end	 

(*

ifThenExp

*)
fun ifThenExp{test, then'} =
let
	val test' = unCx test
	val	then'' = unNx then'
	val t = newlabel()
	val join = newlabel()
in
	Nx ( seq[test'(t, join), 
					LABEL t, 
					then'', 
					LABEL join] )
end


(*

ifThenElseExpUnit

*)
fun ifThenElseExpUnit {test,then',else'} =
let
	val test' = unCx test
	val then'' = unNx then' 
	val else'' = unNx else'
	val t = newlabel() and f = newlabel()
	val join = newlabel()
in
	Nx ( seq [test'(t,f),
				LABEL t,  
				then'', 
				JUMP(NAME join, [join]),
				LABEL f,
				else'', 
				JUMP(NAME join, [join]), 
				LABEL join] )
end 

(*

ifThenElseExp

Pág 162

*)
fun ifThenElseExp {test, then'=(Cx c1), else'=(Cx c2)} = (* Caso en el que aparecen 2 condicionales *)
	let 
		val test' = unCx test
		val t = newlabel() and f = newlabel() 
		fun mkstm (t',f') = 
				seq [ test'(t, f), 
					LABEL t, 
					c1(t', f'),
					LABEL f,
					c2(t', f') ] 
	in 
		Cx mkstm 
	end

  | ifThenElseExp {test, then'=(Cx c), else'= Ex (CONST 0) } = (* Caso en el que aparece un condicional en then *)
	let 
		val test' = unCx test
		val t = newlabel()
		fun mkstm (t',f') = seq [ test'(t, f'), LABEL t, c(t', f') ]
	in
		Cx mkstm
	end
  | ifThenElseExp {test, then'=(Cx c), else'} = 
	let 
		val test' = unCx test
		val else'' = unEx else'
		val t = newlabel() and f = newlabel()
		val z = newlabel() and j = newlabel()
		val r = newtemp()
	in 
		Ex ( ESEQ (  seq [ MOVE(TEMP r, CONST 1),
							test'(t,f), 
							LABEL t, 
							c(j,z), 
							LABEL f, 
							MOVE(TEMP r, else''),
							JUMP(NAME j, [j]), 
							LABEL z,
							MOVE(TEMP r, CONST 0),
							LABEL j],
						TEMP r))
	end
	(* Caso en el que aparece un condicional en else, para este caso
	   Rearmamos test para que sea su negación, y reutilizamos el caso
		anterior para un condicional *)
  | ifThenElseExp {test, then', else'=(Cx c)} = 
	let
		fun test'' (t,f) = unCx test (f,t)
	in
		ifThenElseExp{test=(Cx test''), then'=(Cx c), else'=then'}
	end 

  | ifThenElseExp {test,then',else'} = (* Caso genérico *) 
	let
		val test' = unCx test
		val then'' = unEx then' 
		val else'' = unEx else'
		val t = newlabel() and f = newlabel()
		val join = newlabel()
		val r = newtemp() 
	in
		Ex ( ESEQ (seq [test'(t,f),
							LABEL t, 
							MOVE (TEMP r, then''), 
							JUMP(NAME join, [join]),
							LABEL f,
							MOVE (TEMP r, else''), 
							JUMP(NAME join, [join]), 
							LABEL join],
					TEMP r) ) 
	end

(*

assignExp

Pag 153. 

*)
fun assignExp{var, exp} =
let
	val v = unEx var
	val vl = unEx exp
in
	Nx (MOVE(v,vl))
end

(*

binOpIntExp

*)
fun binOpIntExp {left, oper, right} = 
let
	val l = unEx left
	val r = unEx right
in 
	case oper of
		PlusOp => Ex (BINOP(PLUS,l,r))
	  | MinusOp => Ex (BINOP(MINUS,l,r))
	  | TimesOp => Ex (BINOP(MUL,l,r))
	  | DivideOp => Ex (BINOP(DIV,l,r))
	  | _ => raise Fail "Esto no deberia pasar (6)"
end 

(*

binOpIntRelExp

Pág 161 

*)
fun binOpIntRelExp {left,oper,right} =
let 
	val l = unEx left
	val r = unEx right
in 
	 case oper of 
		EqOp => Cx (fn (t, f) => CJUMP(EQ, l, r, t, f))
	  | NeqOp => Cx (fn (t, f) => CJUMP(NE, l, r, t, f))
	  | LtOp => Cx (fn (t, f) => CJUMP(LT, l, r, t, f))
	  | GtOp => Cx (fn (t, f) => CJUMP(GT, l, r, t, f))
	  | LeOp => Cx (fn (t, f) => CJUMP(LE, l, r, t, f))
	  | GeOp => Cx (fn (t, f) => CJUMP(GE, l, r, t, f))
	  | _ => raise Fail "Esto no  deberia pasar (7)" 
end 

(*

binOpStrExp

Pág 162. Compiler calls a runtime-system function that implements string equality.
This function returns 0 or 1.

*)
fun binOpStrExp {left,oper,right} =
let
	val l = unEx left
	val r = unEx right
	val cmp = externalCall("_stringcmp", [l, r])
in
	case oper of
		EqOp => Cx (fn (t, f) => CJUMP(EQ, CONST 0, cmp, t, f))
	  | NeqOp => Cx (fn (t, f) => CJUMP(NE, CONST 0, cmp, t, f))
	  | LtOp => Cx (fn (t, f) => CJUMP(EQ, CONST ~1, cmp, t, f))
	  | GtOp => Cx (fn (t, f) => CJUMP(EQ, CONST 1, cmp, t, f))
	  | LeOp => Cx (fn (t, f) => CJUMP(NE, CONST 1, cmp, t, f))
	  | GeOp => Cx (fn (t, f) => CJUMP(NE, CONST ~1, cmp, t, f))
	  | _ => raise Fail "Esto no deberia pasar (8)" 
end 



end

