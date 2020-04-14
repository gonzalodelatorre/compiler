structure tigerutils :> tigerutils = struct

open tigertrans
open tigercanon
open tigercodegen
open tigerassem
open tigerframe

		
                (* 

		canonize
		
		*)
		fun canonize x = traceSchedule (basicBlocks (linearize x)) 

		(* 

		divideFrags
		
		*)
		fun divideFrags (strs, fmts) []  = (rev strs, rev fmts)
		  | divideFrags (sl, fl) ((STRING s)::fmts) = divideFrags (s::sl, fl) fmts
		  | divideFrags (sl, fl) ((PROC f)::fmts) = divideFrags (sl, f::fl) fmts

		(* 

		genCanonFmts
		
		*)
		fun genCanonFmts fs = List.map (fn {body = b, frame=f} => {body = canonize b, frame = f}) fs

		(* 

		printString
		
		*)
		fun printString s = (print "\nString Fragment:\n"; print (Ir([STRING s])))
			
                (* 

		printFragments
		
		*)
		fun printFragments fg = 
		  let
			fun printFrag (PROC {body, frame = f}) = (print ("\nFragment \""^(tigerframe.name f)^"\":\n"); print (tigerit.tree body))
			  | printFrag (STRING s) = printString s
		  in
			List.app printFrag fg
		  end

		(* 

		printCanonFmts
		
		*)
		fun printCanonFmts (sl, fl) =
		  let
			fun printCFrag {body, frame = f} = 
				(print ("\nFragment \""^(tigerframe.name f)^"\":\n");
				List.app (fn st => print (tigerit.tree st)) body)
		  in
			List.app printString sl;
			List.app printCFrag fl 
		  end
   
                
		
                (* 


		Generación de instrucciones 


		*)
		


		(* 

		geninstr
		
		*)
		fun geninstr cfmts =
		  let 
			fun geninstr1 _ [] = []
			  | geninstr1 frame (st::stl) = (codegen frame st) @ (geninstr1 frame stl) 
			
			fun geninstr2 {body, frame} = (geninstr1 frame body, frame)
		  in
			List.map geninstr2 cfmts
		  end
		

		(* 

		instr2string
		
		*)
		val instr2string = format (fn t => t)
		

		(* 

		code2string
		
		*)
		fun code2string [] = ""
		  | code2string (instr::l) = (instr2string instr)^(code2string l)
		
		
		(* 

		printCode
		
		*)
		fun printCode instrfrags =
       		  let 
                      fun prIproc (instrlist, frm) = (print ((tigerframe.name frm)^":\n");
						      print (code2string instrlist); 
						      print "\n")
                  in    
		      print "**********  CÓDIGO SIN COLOREAR: **********  \n\n"; 
		      List.app prIproc instrfrags;
		      print "\n\n\n"
                  end
		

		(* 

		allocinstr
		
		*)
		fun allocinstr alloc = 
		  let
			fun fmt t = case (Polyhash.peek alloc t) of 
								    SOME d => d 
								  | NONE => t 
		  in 
			format fmt
		  end
		

                (* 

		genFinal
		
		*)
		fun genFinal alloc instrlist = List.foldr (fn (inst, str) => (allocinstr alloc inst)^str) "\n" instrlist
		
 		(* 

		sameMove
		
		*)	
                fun sameMove alloc (tigerassem.MOVE {assem = assem, src = src, dst = dst}) = 
		                            if (String.compare(Polyhash.find alloc src, Polyhash.find alloc dst) = EQUAL) then false else true
                  | sameMove  _  _  = true
         
end
