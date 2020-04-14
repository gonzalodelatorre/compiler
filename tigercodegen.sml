structure tigercodegen :> tigercodegen = struct

open tigerassem
open tigertree
open tigerframe

exception Multiplode4 

fun codegen frame stm = 
let 

    (* Lista de instrucciones *)	
    val ilist = ref (nil: instr list)

    (*
   
     emit : instr -> unit

     Accumulates a list of instructions to be returned later

    *)
    fun emit x = ilist := x :: !ilist

    val Sp = tigerframe.sp

    fun imm12 x = ((x >= 0) andalso (x <= 4095))    
    
    val immConst = 65536
    
    fun negoffset x = (x < 256)          (*Puede ser 4095 en ARM. Si es Thumb es 256, pongo 256 por las dudas *)

     
    (*

    fn result

    *)
    fun result gen = let 
			val t = tigertemp.newtemp() 
		     in 
			gen t;
			t 
		     end
 
	
     (*
   
     munchStm : tigertree.stm -> unit

     *)
      fun munchStm (SEQ(a,b)) = (munchStm a; munchStm b) 
	  
         | munchStm (CJUMP (relop, e1, e2, lab1, lab2)) =
	    
	    let val instr  = case relop of 
	                               EQ   => "beq" 
	                             | GT   => "bgt"
	                             | LT   => "blt"  (* XXX jump to lab1 in case e1 is lower... *)
	                             | GE   => "bge"
	                             | LE   => "ble"
	                             | NE   => "bne"
	                             | ULT  => "blo" 
	                             | ULE  => "bls"
	                             | UGT  => "bhi"
	                             | UGE  => "bhs"
 	                           
	    in emit (OPER {assem = "cmp     's0, 's1\n" ^ instr ^ " " ^ lab1 ^ "\n",
	                   src = [munchExp e1, munchExp e2], 
                           dst = [],
	                   jump = SOME [lab1, lab2]})
	                   
	    end               

	 | munchStm (JUMP (NAME (lab), _)) =
	   emit (OPER {assem = "b       "^lab^"\n", 
	                src = [], 
			dst = [],
	                jump = SOME [lab] })
	  
	  
	 | munchStm (JUMP ( e1, labels) ) =
	   emit (OPER {assem = "bx      's0\n",  (* ojo con bx,  branch exchange*)
	                src = [munchExp e1], 
			dst = [],
	                jump = SOME labels})
	
	 | munchStm (MOVE(TEMP x, CALL a)) = munchStm(SEQ(EXP(CALL a), MOVE(TEMP x, TEMP tigerframe.rv))) (* Function call *)
	     
         | munchStm (EXP(CALL(NAME lf, args))) = (* Procedure call *)
     
       
       let 
                 

           val argtemps = munchArgs(0, args) 
      
           fun genPush _ [] = () 
             | genPush n (h::t) =(if n < List.length argregs then (* < 4 Usamos Registros *)
                                     (emit(tigerassem.MOVE{assem = "mov     'd0, 's0\n",
							   src = h,
							   dst = List.nth(argregs,n)}) ; 
				     genPush (n+1) t) 
                                  else      			 (* >= 4 Push a stack *)        
                                     (List.app (fn x => (emit(OPER{assem = "push    {'s0}\n", 
								   src=[x], 
								   dst=[], 
							           jump=NONE}))) (List.rev(h::t))  
                                     )

                                  )  
       in
	(* Push argumentos a registros a stack.  TODO  BORRAR*)
        (if (List.length(argtemps) > 0) then genPush 0 argtemps else () );
        emit (OPER {assem = "bl      " ^ lf ^ "\n", 
		    src = List.take(argregs, Int.min(List.length argregs, List.length argtemps)), 
                    dst = calldefs,   
                    jump = NONE}) ;  
              let 
		   val offset = (List.length args - List.length tigerframe.argregs)*tigerframe.wSz 
              in
                   if (offset > 0) then (munchStm (MOVE(TEMP sp, BINOP(PLUS, CONST offset, TEMP sp)))) else ()         
              end   
                    	  
	end
	   
	   
	 (* Munch MOVE a TEMP de + *)  
	 | munchStm (MOVE(TEMP x, BINOP(PLUS, CONST i, TEMP y))) = 
	       if (imm12 i) then (
                   (* Si los temporarios son iguales *)
	           emit(OPER {assem = (if (String.compare(x, y) = EQUAL) then 
	                                  "add    'd0, #" ^ Int.toString i ^ "\n"  
	                               else 
	                                  "add    'd0, 's0, #" ^ Int.toString i ^ "\n"),
	                      src = [y],
	                      dst = [x], 
	                      jump = NONE})
	           
	       ) else (
	           emit(OPER {assem = "add    'd0, 's0, 's1\n",
	                      src = [y, munchExp(CONST i)],
	                      dst = [x], 
	                      jump = NONE})
	           
	       
	       )
      

         (*

	  Accesos a Memoria

         *)         
         | munchStm (MOVE(MEM(BINOP(PLUS, CONST i, TEMP "sp")), e1)) =  
	     
	     (* offset múltiplo de 4 entre 0 y 1020*)
	     ((if (i mod 4 = 0) then () else raise Fail "offset del sp tiene que ser multiplo de 4\n") ;  
	    
	     (* offset  entre 0 y 1020*)
	     (if ((i >= 0) andalso (i <= 1020)) then 
               (* str r2, [r1, #2]  @ address mode: offset. Store the value found in R2 to the memory address found in R1 plus 2. Base register (R1) unmodified. *)
	       (emit(OPER {assem= "str     's0, [sp, #" ^ Int.toString i ^ "]\n", 
	                src = [munchExp e1, sp], 
	                dst = [],
	                jump = NONE}))
	     else 
	       (* str r2, [r1, r2]  @ address mode: offset. Store the value found in R2 to the memory address found in R1 with the offset R2. Base register unmodified. *)
	       (emit(OPER {assem= "str     's0,[sp, 's1]\n",
	                src = [munchExp e1, munchExp(CONST i), sp], 
	                dst = [],
	                jump = NONE})))
	      )            
	  | munchStm (MOVE(MEM(BINOP(PLUS, TEMP "sp", CONST i)), e1))  = munchStm (MOVE(MEM(BINOP(PLUS, CONST i, TEMP tigerframe.sp)), e1)) 
	  
	  | munchStm (MOVE(MEM (BINOP(PLUS, e2, CONST i)), e1)) =

	  if imm12 i then
	     emit(OPER {assem="str     's0, ['s1, #" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1, munchExp e2],
	                dst = [],
	                jump= NONE})
	  else 
	     emit(OPER {assem="str     's0, ['s2, 's1]\n",
	                src = [munchExp e1,munchExp (CONST i), munchExp e2],
	                dst = [],
	                jump= NONE})
	    
	  | munchStm (MOVE(MEM (BINOP(PLUS, CONST i, e2)), e1)) = munchStm (MOVE(MEM (BINOP(PLUS, e2, CONST i)), e1))                                 	 
	                              
	  | munchStm (MOVE(MEM (BINOP(PLUS, e2, e3)), e1)) =
	     emit(OPER {assem="str     's0, ['s1, 's2]\n",
	                src = [munchExp e1,munchExp e2,munchExp e3],
	                dst = [],
	                jump= NONE})  
	  
	 | munchStm (MOVE(MEM (BINOP(MINUS, e2, CONST i)), e1)) =
	  if (negoffset i) then 
	     emit(OPER {assem="str     's0, ['s1, #-" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1, munchExp e2],
	                dst = [],
	                jump= NONE})
	  else 
	     (* str r2, [r1]  @ store the value found in R2 to the memory address found in R1 *)
	     emit(OPER {assem="str     's0, ['s1]\n",
	                src = [munchExp e1, munchExp(BINOP(MINUS, e2, CONST i))],
	                dst = [],
	                jump= NONE})
	  
	  | munchStm (MOVE(MEM e2, e1)) =
	     emit(OPER {assem= "str     's0,['s1]\n",
	                src = [munchExp e1, munchExp e2], 
	                dst = [],
	                jump = NONE}) 
	                
	   | munchStm (MOVE(TEMP t1, e2)) = 
	     emit(tigerassem.MOVE {assem= "mov     'd0, 's0\n",
	                src = munchExp e2,
	                dst = t1})
	  
	  (* Munch LABEL *)
	  | munchStm (tigertree.LABEL lab) = 
	     emit(tigerassem.LABEL {assem = lab ^ ": \n" , lab = lab})	     
	  
	  | munchStm (EXP e) = let val _ = (munchExp e) in () end 	  
	  
	  | munchStm _ = raise Fail "ERROR Caso no cubierto en tigercodegen.munchStm()"               



	and 
       
         
	(*


	fn munchExp


	*)
	   munchExp (ESEQ _) = raise Fail "tigercodegen.munchExp (ESEQ _) no debería pasar el canonizador"   
  	
	 | munchExp (CALL _) = raise Fail "tigercodegen.munchExp (CALL _) no debería pasar el canonizador" 
		
	 
	 | munchExp (MEM (BINOP (PLUS, CONST i, e1))) = 
	    if (imm12 i) then  
	     result(fn r => emit(OPER
		    (* ldr r2, [r1, #2]  @ address mode: offset. Load the value at memory address found in R1 plus 2 to register R2. Base register (R1) unmodified. *)
	            {assem = "ldr     'd0, ['s0, #" ^ Int.toString i ^ "]\n",
	             src = [munchExp e1], 
		     dst = [r],
	             jump = NONE }))
	    else 
	     result(fn r => emit(OPER
	             (* ldr r3, [r1, r2]  @ address mode: offset. Load the value at memory address found in R1 with the offset R2 to register R3. Base register (R1) unmodified. *)
	            {assem = "ldr     'd0, ['s1, 's0]\n",
	             src = [munchExp (CONST i), munchExp e1], 
		     dst = [r],
	             jump = NONE }))      
	 
	| munchExp (MEM (BINOP (PLUS, e1, CONST i))) = munchExp(MEM (BINOP (PLUS, CONST i, e1)))
 
	| munchExp (MEM (BINOP (MINUS, e1, CONST i))) = 
	    if (negoffset i) then  
	     result(fn r => emit(OPER
	            {assem = "ldr     'd0, ['s0, #-" ^ Int.toString i ^ "]\n",
	             src = [munchExp e1], 
		     dst = [r],
	             jump = NONE }))
	    else 
	     result(fn r => emit(OPER
                    (* ldr r2, [r0] load the value at memory address found in R0 to register R2 *)
	            {assem = "ldr     'd0, ['s0]\n",
	             src = [munchExp (BINOP (MINUS, e1, CONST i))], 
		     dst = [r],
	             jump = NONE }))    
	             
	| munchExp (MEM e1) =
	      
	      result(fn r => emit(OPER
	            {assem = "ldr    'd0, ['s0]\n",
	             src = [munchExp e1], 
		     dst = [r],
	             jump = NONE }))                            
	    
	 (* Munch División *)   
	 | munchExp (BINOP (DIV, e1, e2)) = 
		 (munchStm(EXP(CALL (NAME "__aeabi_idiv", [e1, e2]))); 
		  result(fn r => emit(tigerassem.MOVE 
	            {assem = "mov 'd0, 's0\n",
	             src = tigerframe.rv, 
		     dst = r})))
	
	 (* Munch BINOPS *)		
	 | munchExp (BINOP (oper, e1, e2)) =
	  
	      let val op_instr = case oper of
	                            PLUS    => "add "
	                          | MINUS   => "sub "
	                          | MUL     => "mul "
	                          | DIV     => raise Fail "ERROR tigercodegen.munchExp(BINOP (DIV, e1, e2)) ... no debería pasar\n" 
	                          | AND     => "and "
	                          | OR      => "orr "
	                          | XOR     => "eor "
	                          | LSHIFT  => "lsl "
	                          | RSHIFT  => "lsr "
	                          | ARSHIFT => "asr "
	      in
	         
	      result(fn r => emit(OPER 
	            {assem = op_instr ^ "    'd0, 's0, 's1\n",
	             src = [munchExp e1, munchExp e2], 
		     dst = [r],
	             jump = NONE}))
	  
	      end              
	          	             
	  | munchExp (CONST i) =
	     let 
				val bound8 = 256
				val bound16 = 65536
				val assm = if i > 0 andalso i < bound16 then
							(if i < bound8 then 
								"mov 'd0, #" ^ Int.toString i ^ "\n"
								else
								"movw 'd0, #" ^ Int.toString i ^ "\n")
						   else 
							"movw   'd0, #:lower16:" ^ Int.toString i ^ "\n" ^
							"movt   'd0, #:upper16:" ^ Int.toString i ^ "\n"
	      in 
			result(fn r => emit(OPER {assem = assm, 
						  src = [], 
						  dst = [r], 
						  jump = NONE}))

	      end                
	  
	  | munchExp (NAME lab) =
	      result (fn r => emit(OPER
	            {assem = "movw    'd0, #:lower16:" ^ lab ^ "\n" ^ 
                             "movt    'd0, #:upper16:" ^ lab ^ "\n",
	             src = [], 
		     dst = [r],
	             jump = NONE}))
	  
	  | munchExp (TEMP t) = t          
	             
          | munchExp _ = tigertemp.newtemp() 
	
	

	and 

	(* 

	 munchArgs
	 
	 Generates code to move all the arguments to their correct positions. P 204. 

	*)
         munchArgs (_, []) = []
	 |  munchArgs (n, h::t) = (munchExp h) :: munchArgs(n+1, t)   
	   
	

in 	
	munchStm stm; 
	rev(!ilist)
end


end


