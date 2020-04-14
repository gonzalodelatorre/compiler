open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigerutils
open tigertrans
open tigerliveness
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	fun arg(l, s) = (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter")
		val (eabihf,l8)     = arg(l7, "-hf")
		val (arm32, l9)     = arg(l8, "-arm32") 
		val entrada =
			case l9 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opción dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()
		
		val _ = print "\n\n\n"
                val _ = print "**********  AST:  ********** \n\n"
		val _ = tigerpp.exprAst expr
		val _ = print "\n\n\n"
		(* 

		transProg

		*)
		val _ = transProg(expr)
		
		(* 

		Código intermedio 

		*)
		val fragments = getResult()
		val _ = print "\n\n\n"
		val _ = print "**********  CÓDIGO INTERMEDIO:  ********** \n\n" 
		val _ = printFragments(fragments)
		val _ = print "\n\n\n"
		
		
		(* 

                Código intermedio canonizado 

		*)
		val (strings, procfmts) = divideFrags ([], []) fragments
		val canonfmts = genCanonFmts procfmts
		val _ = print "**********  CÓDIGO INTERMEDIO CANONIZADO: **********  \n\n" 
		val _ = printCanonFmts (strings, canonfmts)	
		val _ = print "\n\n\n"
		

		val uncolored_frags = geninstr canonfmts

		(*

		Código sin colorear

		*)
		val _ = printCode uncolored_frags

		
        	(*
		
		bigcolor

		*)
                fun bigcolor (instrlist, frame) =
		   let 
 			(* Append *)
			val il'' = tigerframe.procEntryExit2(frame, instrlist) 
			(* Color. il' instructions, alloc colors *)			
			val (il', alloc) = tigercolor.main (il'', frame)
			(* Remove move instructions where src == dst *)
			val il = List.filter (sameMove alloc) il'
                        (* Creates prologue and epilogue *)
			val {prolog, epilog, ...} = tigerframe.procEntryExit3 (frame, il)
		   in 
			prolog^(genFinal alloc il)^epilog 
	  	   end
     
      
	       (* *)
               val strings_final = concat (List.map tigerframe.genstring strings)
	       
	       (* *)
               val functions_final = concat (List.map bigcolor uncolored_frags)
       
       

                val a32 = if arm32 then " -marm " else ""
       
		val compiler = case eabihf of true => "arm-linux-gnueabihf-gcc -march=armv7-a" ^ a32
					    | false => "arm-linux-gnueabi-gcc -march=armv7-a" ^ a32
	
		val progname = "program.s"

		(* Código final *)
		val finalCode = tigerframe.head_foot (strings_final, functions_final, progname, eabihf) 
			
		val _ = print "**********  CÓDIGO FINAL: **********  \n\n" 
		val _ = print finalCode
		val _ = print "\n\n**********   **********  \n\n" 
		
		val outFile = (TextIO.openOut (progname))
                              handle _ => raise Fail "Falló al abrir el archivo de salida"
                val _ = TextIO.output (outFile, finalCode)
                val _ = TextIO.closeOut outFile
                val _ = Process.system(compiler^" runtime.c "^progname^" -o a.out")
	in
		
		print "yes!!\n"
	end	
		handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
