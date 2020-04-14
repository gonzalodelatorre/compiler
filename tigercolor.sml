structure tigercolor :> tigercolor = 
struct

open tigerpila
open tigerliveness
open tigerset

(* Definición Nodo y Move *)
type node = tigertemp.temp
type move = (node * node)
type nodeSet = node set
type moveSet = move set

(* *)
type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table

(*

getSpilled

Returns colors and spilled nodes. XXX

*)
fun getSpilled (instrlist, frame) = 
let

 (*
  COLORES K 
 *)
 val KCONST = List.length(tigerframe.usable) (* XXX 11 *)

 (* Printer *)
 fun printWL wl = List.app (fn tmp => print (tmp^"\n")) (listItems wl)

 (* Equal Nodos *)
 fun nodeEq (a,b) = (String.compare(a,b) = EQUAL)

 (* Orden .  
 Returns : LESS, EQUAL, o GREATER
 *)
 fun nodeCmp (a,b) = String.compare(a,b)

 (*
 moveEq
 *)
 fun moveEq ((a,b),(c,d)) = nodeEq(a,c) andalso nodeEq(b,d)

 (*
 moveCmp
 *)
 fun moveCmp ((a,b),(c,d)) = case nodeCmp(a,c) of EQUAL => nodeCmp(b,d)
                                                       |  x  => x

 (*
 nodelistToSet : string list -> string set ref 

 Convierte una lista a un conjunto
 *)
 fun nodelistToSet l = 
  let 
	val newset = tigerset.newEmpty nodeCmp 
  in 
	tigerset.addList(newset,l); 
	newset 
  end


 (*


 Estructuras de datos para nodos y moves. Pág 242


 *)

 (* 
 Nodos. "Work lists", set and stacks. 
 *)

 (* Machine registers preassigned a color *)
 val precolored : nodeSet = nodelistToSet tigerframe.usable 
 
 val specialreg : nodeSet = nodelistToSet tigerframe.specialregs 
 
 (* Temporary registers, not precolored and not yet processed *)
 val initial          : nodeSet = tigerset.newEmpty nodeCmp 
 
 (* Low degree non-move related nodes *)
 val simplifyWorklist : nodeSet = tigerset.newEmpty nodeCmp 
 
 (* Low-degree move-related nodes *)
 val freezeWorklist   : nodeSet = tigerset.newEmpty nodeCmp 
 
 (* High degree nodes *)
 val spillWorklist    : nodeSet = tigerset.newEmpty nodeCmp 
 
 (* Nodes marked for spilling *)
 val spilledNodes     : nodeSet = tigerset.newEmpty nodeCmp 
 
 (* Registers that have been coalesced. When u <- v is coalesced, v is added to this set and u put back on some work-list. Or vice versa *)
 val coalescedNodes   : nodeSet = tigerset.newEmpty nodeCmp 
 
 (* Nodes succesfully colored *)
 val coloredNodes     : nodeSet = tigerset.newEmpty nodeCmp  
 
 (* 
 Pila y conjunto 
 *)
 
 (* Stack containing temporaries removed from the graph *)
 val selectStack      : nodeSet = tigerset.newEmpty nodeCmp  
 (* Queue containing temporaries removed from the graph *)
 val selectPila       : node Pila = nuevaPila()  

 (* 
 Move sets.
 *)
 
 (* Moves enabled for possible coalescing *)
 val worklistMoves    : moveSet = tigerset.newEmpty moveCmp 
 
 (* Moves not yet ready for coalescing *)
 val activeMoves      : moveSet = tigerset.newEmpty moveCmp 
 
 (* Moves that will no longer be cosiderer for coalescing *)
 val frozenMoves      : moveSet = tigerset.newEmpty moveCmp 
 
 (* Moves whose source and target interfere *) 
 val constrainedMoves : moveSet = tigerset.newEmpty moveCmp 
 
 (* Moves that have been coalesced *)
 val coalescedMoves   : moveSet = tigerset.newEmpty moveCmp 


 (*
  Other data structures.
 *)

 exception ErrorDegree
 exception ErrorAdjList
 exception ErrorMoveList
 exception ErrorAlias
 exception ErrorColor
 exception ErrorCount

 (* A hashtable containing the current degree of each node *)
 val degree : (node,int) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash, nodeEq) (1000, ErrorDegree) 

 (* The set of interference edges (u,v) in the graph *)
 val adjSet : (node * node) tigerset.set = tigerset.newEmpty moveCmp

 (* Adjacency list representation of the graph. For each non-precolored temporary u, adjList[u] is the set of nodes that interfere with u *)
 val adjList : (node, nodeSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash, nodeEq) (1000, ErrorAdjList) 

 (* Mapping from a node to the list of moves it is associated with *)
 val moveList : (node, moveSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash, nodeEq) (1000, ErrorMoveList)

 (* When a move (u,v) has been coalesced, and v put in coalescedNodes, then alias(v) = u *)
 val alias : (node, node) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash, nodeEq) (1000, ErrorAlias)

 (* The color chose by the algorithm for a node *)
 val color : (node, tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash, nodeEq) (1000, ErrorColor)

 (* Used by heuristic *)
 val usedefcount : (node, int) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash, nodeEq) (1000, ErrorCount)

 (*
 
 debugfind: (string, 'a) hash_table -> string -> 'a
 
 *)
 fun debugfind ht i = case Polyhash.peek ht i of SOME d => d
                                             |  NONE => (print i; Polyhash.find ht i) 
 (* 
 
 findinitNS: (a', string set ref) hash_table -> 'a -> string set ref
 
 *)
 fun findinitNS ht i = 
   let 
     val ne : nodeSet = newEmpty(nodeCmp) 
   in 
     case Polyhash.peekInsert ht (i, ne) of SOME s => s
                                         | NONE => ne  
   end


 (*  (a', int) hash_table -> a'-> int 

   peekInsert htbl (k, d)
   Inserts data d for key k, if k is not
   already in the table, returning NONE.  If k is already in the
   table, and the associated data value is d', then returns SOME d'
   and leaves the table unmodified. 
 *)
 fun findinitI ht i = case Polyhash.peekInsert ht (i, 0) of SOME n => n
                                                         |  NONE => 0
 (* 
 
 findinitMS: (a', (string * string) set ref) hash_table -> 'a -> (string * string) set ref 
 
 *)                                                       
 fun findinitMS ht i = 
	let 
	  val ne : moveSet = newEmpty(moveCmp) 
        in 
           case Polyhash.peekInsert ht (i, ne) of SOME s => s
                                               | NONE => ne  
        end    


 (* 

 adjacent 

 Self-explanatory

 *)
 fun adjacent (v) = difference(findinitNS adjList v, union(selectStack, coalescedNodes)) 

 (* adj_app
  
    Aplica p a todos los nodos adyacentes a v, si se cumple cierta condicion.

 *)
 fun adj_app v p = tigerset.app (fn x => if not(tigerset.member(selectStack, x) orelse tigerset.member(coalescedNodes, x)) then p x else ()) (findinitNS adjList v)  

 (* 

 adj_fold 

 Implements foldl to adjList
 
 *)
 fun adj_fold v f e = tigerset.fold (fn (x, b) => if not(tigerset.member(selectStack, x) orelse tigerset.member(coalescedNodes, x)) then f(x, b) else b) e (findinitNS adjList v)

 (* 
  push

  Push selectStack and selectPila
 *)
 fun push v = (pushPila selectPila v; tigerset.add (selectStack, v))

 (* 
  pop

  Pop selectStack and selectPila
 *)             
 fun pop() = 
  let 
   val n = topPila(selectPila)
  in
   popPila(selectPila);
   delete(selectStack, n);
   n             
  end


 (* 
  peekOrEmpty: ('a, 'b set ref) hash_table -> 'a -> ( 'b * 'b -> order ) -> 'b set ref
 *)
 fun peekOrEmpty table element comparacion = case Polyhash.peek table element  of
			 SOME s => s
			| NONE => tigerset.newEmpty comparacion


 (* 
 
  AddEdge 

  Add edges to adjSet 
 
 *)
 fun AddEdge(u, v) =
   if not(member(adjSet, (u, v))) andalso not(nodeEq(u, v)) then (
      addList(adjSet, [(u, v), (v, u)]) ; (
      if not(member(precolored, u)) then (
         add(findinitNS adjList u, v) ;
         Polyhash.insert degree (u, ((findinitI degree u) + 1))) else () ;
      if not(member(precolored, v)) then (
         add(findinitNS adjList v, u) ;
         Polyhash.insert degree (v, (findinitI degree v) + 1)) else ())
   ) else ()        
      

 (*
  
  NodeMoves
 
 *)
 fun NodeMoves(n) = intersection((findinitMS moveList n), union(activeMoves, worklistMoves)) 

 (* 
 
  MoveRelated 

  Devuelve true si un nodo esta relacionado con algún move

 *)
 fun MoveRelated(n) = not(isEmpty(NodeMoves(n)))


 (* 
  
  EnableMoves
 
  Habilita nodos para posible coealescing 

 *)
 fun EnableMoves n =
   let 
     fun body m = 
       if tigerset.member(activeMoves, m) then
         (tigerset.delete(activeMoves, m);
         tigerset.add(worklistMoves, m))
       else   
         ()
   in           
     tigerset.app body (NodeMoves(n))
   end

 (* 

  DecrementDegree

 *)
 fun DecrementDegree m  = 
   let 
     val d = (findinitI degree m) 
    in
     (* Disminuyo en 1 el degree *)
     Polyhash.insert degree (m, d-1);
     if d = KCONST then (
       (* Habilito *)
       adj_app m EnableMoves; 
       EnableMoves m;
       (* Remuevo de spillWorkList *)
       tigerset.delete (spillWorklist, m);
       if MoveRelated(m) then
          tigerset.add (freezeWorklist, m)
       else
          tigerset.add (simplifyWorklist, m)   
     ) else () 
   end
     
 (* 
 
  Simplify

  Remueve un elemento de la lista a simplificar, agrega en el stack y baja los grados a los adyacentes.

 *)  
 fun Simplify () = 
  let 
    val v = tigerset.oneElem(simplifyWorklist) 
  in
     push v;
     tigerset.delete (simplifyWorklist, v);    
     adj_app v DecrementDegree 
  end      
 
 (* 

  GetAlias

  Devuelve el alias

 *)      
 fun GetAlias (n) = if member(coalescedNodes, n) then GetAlias(Polyhash.find alias n) else n  

 (* 

  AddWorkList

  Actualiza simplifyWorklist

 *)
 fun AddWorkList(u) = 
   if not(member(precolored, u)) andalso not(MoveRelated(u)) andalso (findinitI degree u < KCONST) then
      (delete(freezeWorklist, u);
      add(simplifyWorklist, u)) 
   else ()

 (* 

 OK

 Implemets the heuristic used for coalescing a precolored register

 *)
 fun OK (t, r) = (findinitI degree t) < KCONST orelse member(precolored, t) orelse member(adjSet, (t, r)) 

 (* 

 Conservative 

 Implements the conservative coalescing heuristic.

 *)
 fun Conservative (nodes) = 
  let 
   fun count (x, i)  = if (findinitI degree x >= KCONST) then (i+1) else i
  in
    (tigerset.fold count 0 nodes) < KCONST
  end 

 (*

 condition1

 *)
 fun condition1 (u, v) = adj_fold v (fn (t,b) =>  b andalso OK(t, u)) true 

 
 (*

 condition2

 *)
 fun condition2 (u, v) = Conservative(union(adjacent(u), adjacent(v)))
 
 (* 

  Combine

 *)
 fun Combine(u, v) = 
  let 
    val movelistu = Polyhash.find moveList u 
    val movelistv = Polyhash.find moveList v
    val moveunion = union(movelistu, movelistv) 
  in
    (if member(freezeWorklist, v) 
     then
       delete(freezeWorklist, v)
     else 
      delete(spillWorklist, v)
     );
    add(coalescedNodes, v);
    Polyhash.insert alias (v, u);
    Polyhash.insert moveList (u, moveunion);
    EnableMoves(v);
    adj_app v (fn t => (AddEdge(t,u); DecrementDegree(t)));
   if (findinitI degree u >= KCONST) andalso (member(freezeWorklist, u)) 
   then 
     (delete(freezeWorklist, u);
      add(spillWorklist, u)) 
   else ()
  end       

 (* 

  Coalesce

  Coalesce phase.

 *)
 fun Coalesce () = 
  let
    val (x, y) = oneElem(worklistMoves) 
    val (x', y') = (GetAlias(x), GetAlias(y))
    val (u, v) = if member(precolored, y') then (y', x') else (x', y') 
  in
    delete(worklistMoves, (x, y));
    if nodeEq(u,v) then  
       (add(coalescedMoves, (x, y));
       AddWorkList(u)
    ) else if (member(precolored, v) orelse member(adjSet, (u, v))) then ( 
       add(constrainedMoves, (x, y));
       AddWorkList(u);
       AddWorkList(v) 
    ) else if (member(precolored, u) andalso condition1 (u, v)) orelse (not(member(precolored, u)) andalso condition2(u, v)) then (
         add(coalescedMoves,(x, y));
         Combine(u, v);
         AddWorkList(u)
      ) else 
         (* No es posible coleasce ahora *) 
         add(activeMoves, (x, y)) 
  end       
 
 (* 

  FreezeMoves


 *) 
 fun FreezeMoves(u) = 
  let 
    fun body (x, y) = 
      let 
         val v = if nodeEq(GetAlias(y), GetAlias(u)) then GetAlias(x) else GetAlias(y)
      in
        delete(activeMoves,(x, y));
        add(frozenMoves,(x, y));
         if (isEmpty(NodeMoves(v))) andalso (findinitI degree v < KCONST) 
         then
            (delete(freezeWorklist, v);
            add(simplifyWorklist, v))
         else 
            ()
      end     
  in
     tigerset.app body (NodeMoves(u))
  end           

 (* 

  Freeze

  Freeze phase.

 *)
 fun Freeze() = 
  let 
    val u = oneElem freezeWorklist 
  in
    delete(freezeWorklist, u);
    add(simplifyWorklist, u);
    FreezeMoves(u)
 end
 
 (* 

 spillcost

 *)
 fun spillcost n = Real.fromInt(Polyhash.find usedefcount n) / Real.fromInt(findinitI degree n) 

 
 (* 

 spillheuristic

 *)
 fun spillheuristic() = 
  let 
    val x = oneElem spillWorklist 
    val (spillnode, _ ) = fold (fn (n, (min, mincost)) => if (spillcost n < mincost) then (n, spillcost n) else (min, mincost)) (x, spillcost x) spillWorklist
  in
    spillnode
  end

 (* 

  SelectSpill

  Spill phase.

 *)
 fun SelectSpill() = 
  let 
    val m = spillheuristic() 
  in
    delete(spillWorklist, m);
    add(simplifyWorklist, m);
    FreezeMoves(m)
  end                                      

 exception Special

 (* 

  AssignColors

  Select phase.

 *)
 fun AssignColors() = 
   let 
     fun body() =
       let 
          val n = pop()
          val okColors = nodelistToSet tigerframe.usable
          
          fun rmvColors w = (if (member(coloredNodes, GetAlias(w)) orelse member(precolored, GetAlias(w))) 
			     then (
                                  let 
 			  	    val wcolor = Polyhash.find color (GetAlias(w)) 
			          in
                                    ((if not(member(specialreg, wcolor)) then delete(okColors, wcolor) else ()))
                                  end
                                    )
                              else () )
          in
             if not(member(specialreg,n))
             then (tigerset.app rmvColors (Polyhash.find adjList n);
             
               
               if isEmpty(okColors) 
               then
		(* Spill real *)
                add(spilledNodes, n) 
               else ( 
 		(* Agrego color *)
                add(coloredNodes, n);
                Polyhash.insert color (n, oneElem(okColors)) 
                     ) )
             else ()   
          end  
                          
       fun coalescedcolor n = Polyhash.insert color (n, (Polyhash.find color (GetAlias(n)))) 
                            
  in                     
  (*
   XXX
   print "SelectStack : \n" ;       
   printWL selectStack ;
   print "\n\n" ;
   *)
   (while not(isEmpty(selectStack)) do body());
   tigerset.app coalescedcolor coalescedNodes
  end
    



 (* 

  printTable (string, string) hash_table -> Unit

  Hash table pretty printer 

 *)
 fun printTable ht = Polyhash.apply (fn (x, d) => (print x; print "->"; print d; print "\n" )) ht

 

										
 (* 

  MakeWorklist

  Actualiza workLists

 *)
 fun MakeWorklist () = 
  let
	fun selectWL tmp = 
	
	  let
		val deg = findinitI degree tmp
	  in
		if deg >= KCONST
		then tigerset.add(spillWorklist, tmp)
		else if MoveRelated(tmp) 
		     then tigerset.add(freezeWorklist, tmp)
	             else tigerset.add(simplifyWorklist, tmp)   
	  end
  in
	tigerset.app selectWL initial
  end


 (* Flowgraph and list of nodes - instructions *)
 val (flowgraph, ilist) = tigerflow.instrs2graph instrlist
 val (tigerflow.FGRAPH{control, def, use, ismove}) = flowgraph

 (* Liveness Analysis *)
 val (interferencegraph, liveout) = tigerliveness.interferenceGraph flowgraph
	

 (* 

 Build

 *)
 fun Build () = 
	let
		(* Valores vivos en cada instrucción, empezando por la última *)
		fun procInstr instr = 
		  let
			val live = tigerset.difference (nodelistToSet (liveout instr), specialreg) 
			val ismove' = Splaymap.find(ismove, instr)  
			val use' = tigerset.difference (nodelistToSet(Splaymap.find(use, instr)), specialreg) 
			val def' =  tigerset.difference (nodelistToSet(Splaymap.find(def, instr)), specialreg)
			
			(*

			addToMoveList

			*)
			fun addToMoveList n mv = 
			  let  
				val moveList_n = peekOrEmpty moveList n moveCmp
			  in 
				tigerset.add(moveList_n, mv);
				Polyhash.insert moveList (n, moveList_n)
			  end
			
			(*

			count

			*)
			 fun count t = 
				case (Polyhash.peekInsert usedefcount (t, 1)) of 
						 SOME d => Polyhash.insert usedefcount (t, d+1)
			                       | NONE   => ()
			   
		  in 	
		    (* Cuenta use y def *)
		    app count (union(use', def'));
		    
		    (* Agrego nodos a initial *)
		    initial := !(tigerset.union(initial, union(use', def')));
			
                    (* Resto del build *)
		    if (ismove' andalso notEmpty(use') andalso notEmpty(def')) then 
			let
			    val src = oneElem use'
			    val dst = oneElem def'
			in 
			    (if member(live, src) then tigerset.delete(live, src) else ();
			    addToMoveList src (dst, src); 
			    addToMoveList dst (dst, src);
			    add (worklistMoves, (dst, src)))
			end
		    else ();

		    (* *)
		    app (fn d => app (fn l => AddEdge(l, d)) live) def'
		 end
	in  
		List.app procInstr (rev (tigergraph.nodes control)); 
		initial := !(difference(initial, precolored))
	end


 (*

 Loop

 Main loop 

 *)	
 fun Loop() =  
  (if notEmpty(simplifyWorklist) then (Simplify(); Loop())
  else if notEmpty(worklistMoves) then (Coalesce(); Loop())
  else if notEmpty(freezeWorklist) then (Freeze(); Loop())
  else if notEmpty(spillWorklist) then (SelectSpill(); Loop()) else ())	


	
    
 (*

 Init

 *)
 fun Init () = 
           (app (fn x => Polyhash.insert color (x, x)) precolored;
	    app (fn x => Polyhash.insert color (x, x)) specialreg)
	
in
    Init ();
    

    Build (); 


        print "********** COLOR: **********  \n\n"; 
	print "\n\nflow graph:\n\n";
        tigerflow.debugflowprint(flowgraph, ilist, instrlist);

        print "\n\nliveout:\n\n";
        tigerliveness.printLiveOut(flowgraph);

	print "\n\ninterfence graph:\n\n";
	tigerliveness.show(interferencegraph); 

	print "\n\ninitial:\n";
	printWL initial;
        print "\n\nprecolored:\n";
	printWL precolored;

    MakeWorklist ();

	print "\n\nsimplifyWorklist:\n";
	printWL simplifyWorklist;
	print "\n\nfreezeWorklist:\n";
	printWL freezeWorklist;
	print "\n\nspillWorklist:\n";
	printWL spillWorklist;

	print "\n\nBefore loop...\n";
    Loop(); 
    
        print "\n\nAfter loop...\n";

    AssignColors();


       print "\n\ncolor:\n";
       printTable color; 
       print "\n\nspilledNodes:\n";
       printWL spilledNodes;  
       print "\n\n********** END COLOR: **********  \n\n";

 
    (color, spilledNodes)
end


 (*

 main

 Loops until no spills are generated 

 *)
 fun main (instrlist, frame) = 
  let
    val (color, spilledNodes) = getSpilled (instrlist, frame)
  in
    (if notEmpty(spilledNodes) 
     then
         let
            val (newinstrlist, _ ) = tigerregalloc.spill (spilledNodes, frame, instrlist) 
         in
            main (newinstrlist, frame)
         end
     else (instrlist, color)) 
  end    
    
end





