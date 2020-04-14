structure tigerflow :> tigerflow =
struct
    open tigertab  
    open tigerassem
    open tigergraph 

    (*
     
    Managess control-flow graphs. P223.
    
    *)
    datatype flowgraph =
   
    FGRAPH of {control : tigergraph.graph, 		     (* Directed graph wherein each node represents an instruction *)
               def : (tigertemp.temp list) tigergraph.table, (* Table of temporaries defined at each node - Destination registers of the instruction *)
               use : (tigertemp.temp list) tigergraph.table, (* Table of temporaries used at each node - Source registers of the instruction *)
               ismove : bool tigergraph.table}               (* Tells whether each instruction is a MOVE instruction *)
               
   (* 

    instrs2graph : tigerassem.instr list -> flowgraph * tigergraph.node list

    Takes a list of instructions and returns a flow graph along with a list of nodes that
    corresponds exactly to the instructions

   *)              
   fun instrs2graph instr_list = 
    
   let 
      
      val control = newGraph() 
      
      (*

      node_list

      graph * node' list

      *)
      val node_list = List.map (fn _ => newNode(control)) instr_list 

     
      (*

      useFromInstr

      *)
      fun useFromInstr (node, OPER { src, ... }, tabla) = Splaymap.insert (tabla, node, src)  
         | useFromInstr (node, MOVE { src, ... }, tabla) = Splaymap.insert (tabla, node, [src])
         | useFromInstr (node, LABEL { ... }, tabla) = Splaymap.insert(tabla, node, [])
              
           
      (*

      defFromInstr

      *)      
      fun defFromInstr (node, OPER { dst, ... }, tabla)  = Splaymap.insert (tabla, node, dst)  
        | defFromInstr (node, MOVE { dst, ... }, tabla) = Splaymap.insert (tabla, node, [dst])
        | defFromInstr (node, LABEL { ... }, tabla) = Splaymap.insert (tabla, node, [])
 

      (*

      moveFromInstr

      *)
      fun moveFromInstr (node, OPER {... }, tabla) = Splaymap.insert (tabla, node, false)  
        | moveFromInstr (node, MOVE { ... }, tabla) = Splaymap.insert (tabla, node, true)
        | moveFromInstr (node, LABEL { ... }, tabla) = Splaymap.insert (tabla, node, false)
  


      (*

      use
     
      *)   
      val use : (tigertemp.temp list) table = ListPair.foldr useFromInstr (newTable()) (node_list, instr_list)  


      (*

      def
     
      *)
      val def = ListPair.foldr defFromInstr (newTable()) (node_list, instr_list)
	  

      (*

      ismove
     
      *)
      val ismove = ListPair.foldr moveFromInstr (newTable()) (node_list, instr_list)
	  
  

      (*

       addFlowEdges
    
      *)
      fun addFlowEdges () = 
        let 
         
         (* Armo diccionario de label -> nodo *)
          val labDict : (label, node) Splaymap.dict =
	      
              let    
	    
	         val vacio : (label, node) Splaymap.dict = Splaymap.mkDict (String.compare) 
                  
                 fun labFromInstr (node, OPER { ... }, tabla)  = tabla  
                   | labFromInstr (node, MOVE { ... }, tabla) = tabla
                   | labFromInstr (node, LABEL { lab, ... }, tabla) = Splaymap.insert (tabla, lab, node)
              in
          
                 ListPair.foldr labFromInstr vacio (node_list, instr_list)	     
	      
	      end  
          
          (* 

	  jmpFromInstr

	  Agrega aristas nodo -> nodo mirando los labels de los jmp. Si no, agrega la arista al próximo

          *)
          fun jmpFromInstr (node, next, OPER { jump = SOME (lablist), ... }) dict = (List.app  (fn x => mk_edge ({from = node, to = Splaymap.find(dict, x)})) lablist)
            | jmpFromInstr (node, next, OPER { jump = NONE, ... }) dict = mk_edge ({from = node, to = next})  
            | jmpFromInstr (node, next, MOVE { ... }) dict = mk_edge ({from = node, to = next}) 
            | jmpFromInstr (node, next, LABEL { ... }) dict = mk_edge ({from = node, to = next}) 
      
      
          fun zip3 a b [] = []
            | zip3 a [] c = []
            | zip3 [] b c = []
            | zip3 (a::ra) (b::rb) (c::rc) = (a, b, c) :: (zip3 ra rb rc)
            
        in 
      
         List.app (fn x => jmpFromInstr x labDict) (zip3 node_list (List.tl node_list) instr_list)  
       
        end

    in 
       
       addFlowEdges();
       (FGRAPH{control = control, def = def, use = use, ismove = ismove}, node_list)
    
    end     

    exception UnequalLenghts
    exception ControlUnequal
    

    (* 

    instrToString  

    *)
    val instrToString = tigerassem.format (fn t => t)

    (* 

    debugflowprint : (flowgraph * tigergraph.node list * tigerassem.instr list) -> unit  

    *)
    fun debugflowprint(FGRAPH{control = control, def = def, use = use, ismove = ismove}, node_list, instr_list) = 
       let 
           val _ = if (List.length(node_list) <> List.length(instr_list)) then (raise (UnequalLenghts)) else ()
           val _ = if (List.length(nodes(control))) <> List.length(instr_list) then raise (ControlUnequal) else () 
           
           fun printTemp t = (print t; print ",")
           
           fun printIsMove b = (if b then print "isMove" else print "notMove")
           
           fun printnodeinstr (n, i) = (print (nodename n); print ": "; print (instrToString i);
                                       print "def: "; List.app printTemp (Splaymap.find(def, n)); 
                                       print "use: "; List.app printTemp (Splaymap.find(use, n)); 
                                       printIsMove (Splaymap.find(ismove, n)); print "\n")

           fun printEdges n = List.app (fn x => (print (nodename n) ; print " --> "; print (nodename x); print "\n")) (succ n)                              
           
       in
               print "Nodos : \n" ;
               ListPair.app printnodeinstr (node_list, instr_list);
               print "\n\nAristas : \n";
               List.app printEdges (nodes control);
               print "\n\n\n"
       end

end

