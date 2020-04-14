structure tigerregalloc :> tigerregalloc =
struct
open tigerassem
  
  exception ErrorOffset
  exception ErrorAccess

  type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table
   
  val imm = 256
   
   (*

   Spill 

   *)
   fun spill (spilledNodes, frame, instrList) = 
      let
          (* Nueva lista de instrucciones *)
          val ilist = ref ([]:(instr list))
          fun emit x = ilist := x::(!ilist)
         
          
          (* Conjunto de temporarios nuevos *)
          val newTemps = tigerset.newEmpty(String.compare) 
          
          (* Code gen Store and fetch instr *)

          (*

	  storeTemp

	  *)
          fun storeTemp(temp, mempos) =
	      if abs(mempos) < imm 
              then ( 
		    let
			val desp = if mempos < 0 then "-" ^ Int.toString(~mempos) else if mempos >= 0 then "+" ^ Int.toString(mempos) else ""
		    in
			emit(OPER {assem= "str     's0, [fp,#" ^ desp ^ "]\n", src=[temp, tigerframe.fp], dst=[], jump=NONE}) 
		    end
             ) else (
                let 
                   val newt = tigertemp.newtemp()
                   val movestr = "movw    'd0, #" ^ Int.toString(abs(mempos)) ^ "\nmovt    'd0, #" ^ Int.toString(abs(mempos)) ^ "\n" 
                   val assemstr = if mempos < 0 then "sub     'd0, fp, 'd0\n"  else "add     'd0, fp, 'd0\n"
                in
                   tigerset.add (newTemps, newt);
                   emit(OPER {assem = movestr ^ assemstr, src = [tigerframe.fp], dst = [newt], jump = NONE});
                   
                   emit(OPER {assem = "str     's0, ['s1]\n", src = [temp, newt, tigerframe.fp], dst = [], jump = NONE})
                end                  
              )
          
          (*

	  fetchTemp

	  *)
          fun fetchTemp(temp, mempos) =
	    if (abs(mempos) < imm)
            then (
	       let
		 val desp = if mempos < 0 then "-" ^ Int.toString(~mempos) else if mempos >= 0 then "+" ^ Int.toString(mempos) else ""
	       in
		 emit(OPER {assem = "ldr     'd0, [fp,#" ^ desp ^ "]\n", src = [tigerframe.fp], dst = [temp], jump = NONE})
	       end
          ) else (
                let 
                   val newt = tigertemp.newtemp()
                   val movestr = "movw   'd0, #" ^ Int.toString(abs(mempos)) ^ "\nmovt   'd0, #" ^ Int.toString(abs(mempos)) ^ "\n" 
                   val assemstr = if mempos < 0 then "sub    'd0, fp, 'd0\n"  else "add    'd0, fp, 'd0\n"
                in
                   tigerset.add (newTemps, newt);
                   emit(OPER {assem = movestr ^ assemstr, src = [tigerframe.fp], dst = [newt], jump = NONE});
                   emit(OPER {assem = "ldr    'd0, ['s0]\n", src = [newt, tigerframe.fp], dst = [temp], jump = NONE})    
               end
          )
          
          (* Tabla de temp spilleados a su mempos en el frame *)
          val offset : (tigertemp.temp, int) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash, (fn (a, b) => String.compare(a, b) = EQUAL)) (100, ErrorOffset)  
          
          (* 
	
          accToInt : access->int

	  Saca constructor InFrame

	  *)
          fun accToInt a = case a of tigerframe.InFrame i => i
                                   | tigerframe.InReg _ => raise ErrorAccess
          
          
          (* 

          allocate
   
          Crea espacio en frame para el temp t y lo agrega a la tabla offset 
 
          *)
          fun allocate t = Polyhash.insert offset (t, accToInt(tigerframe.allocLocal frame true)) 
        
     
          (* 

	  genNewTemps 

	  Retorna la lista de temps con modificaciones y la lista de pares (tempnuevo, mempos) 
          para nuevas instrucciones 

          *)      
          fun genNewTemps (t, (ts, newtmem)) = case (Polyhash.peek offset t) of 
									SOME mempos => 
                                                                              (let 
    										 val t' = tigertemp.newtemp() 
									       in 
                                                                                 (tigerset.add (newTemps, t); 
                                                                                 ((t'::ts), (t', mempos)::newtmem)) 
                                                                               end)
                                                                        |NONE => (t::ts, newtmem)                                  
                                          
          (*

	  procInstr

	  Procesa cada instrucción

	  *)
          fun procInstr (OPER {assem = assem, src = src, dst = dst, jump = jump}) = 
             let   
               val (src', news) = foldr genNewTemps ([], []) src
               val (dst', newd) = foldr genNewTemps ([], []) dst                                                                                                                 
             in
                List.app fetchTemp news;
                emit(OPER {assem = assem, src = src', dst = dst', jump = jump});
                List.app storeTemp newd
             end
          
          | procInstr (MOVE {assem = assem, src = src, dst = dst}) = 
             let
               val (src', news) = foldr genNewTemps ([], []) [src]
               val (dst', newd) = foldr genNewTemps ([], []) [dst]     
             in
                List.app fetchTemp news;
                emit(MOVE {assem = assem, src = List.hd src', dst = List.hd dst'});
                List.app storeTemp newd
            end
          | procInstr a = emit(a)   
            
   
      in    
          tigerset.app allocate spilledNodes;
          
          List.app procInstr instrList;
          
          (rev(!ilist), newTemps)
      end
end      
