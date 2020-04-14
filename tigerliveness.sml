structure tigerliveness :> tigerliveness = 
struct
open tigertemp
open tigerflow
open tigergraph
open Splaymap
open Splayset


datatype igraph = 
		IGRAPH of {graph: tigergraph.graph,			    (* Interference graph *)
			   tnode: tigertemp.temp -> tigergraph.node,        (* Mapping from temporaries of the assem program to graph nodes *)
		           gtemp: tigergraph.node -> tigertemp.temp,        (* Inverse Mapping *)
		           moves: (tigergraph.node * tigergraph.node) list} (* List of move instructions *)


(* liveness *)				
type liveness = (tigergraph.node -> tigertemp.temp list) 

(* liveSet *)
type liveSet = temp Splayset.set

(* Given a flow-graph node n, the set of live temporaries at that node *)
type liveMap = liveSet tigergraph.table

val lIn = ref (newTable () : liveMap)
val lOut = ref (newTable () : liveMap)

(*

emptyLiveSet 

Creates a new empty set

*)
fun emptyLiveSet () = Splayset.empty String.compare

(*

list2set

val addList : '_item set * '_item list -> '_item set

Adds all items from the list lst to the set empty() 

*)
fun list2set empty lst = Splayset.addList (empty(), lst)


(*

stringlist2set

*)
fun stringlist2set lst = list2set emptyLiveSet lst


(*

curry : ( a * b -> c ) -> a -> b -> c

*)
fun curry f = fn x => fn y => f (x,y)


(*

listEq

*)
fun listEq ([],[]) _ = true
  | listEq (x::lx, y::ly) cmp = cmp(x,y) andalso (listEq (lx,ly) cmp)
  | listEq _ _ = false

(*

liveSetCmp

*)
fun liveSetCmp ((_, s1: liveSet), (_, s2: liveSet)) = Splayset.equal(s1, s2)

(*

liveMapEq

*)
fun liveMapEq (m1 : liveMap, m2 : liveMap) = listEq (Splaymap.listItems m1, Splaymap.listItems m2) liveSetCmp

(*

peekOrEmpty

*)
fun peekOrEmpty dict el = case (Splaymap.peek (dict,el)) of 
                                  SOME s => s
                                | NONE => emptyLiveSet()

(*

calcLiveness

Calcula lIn y lOut. P213 

*)
fun calcLiveness (FGRAPH {control, def, use, ismove}) = 
  let
	fun intGraph (livein : liveMap, liveout : liveMap) = 
	  let 
		fun intNode (node, (lin, lout)) = 
		    let
			
			val inSet = peekOrEmpty lin node
			val outSet = peekOrEmpty lout node
			val defSet = stringlist2set (Splaymap.find (def, node))
			val useSet = stringlist2set (Splaymap.find (use, node))
			val inSet' = union (useSet, (difference (outSet, defSet)))
			val listOfSets' = List.map (peekOrEmpty lin) (succ node)
			val outSet' = List.foldr union (emptyLiveSet ()) listOfSets'
                    in 
			(Splaymap.insert(lin, node, inSet'), 
			 Splaymap.insert(lout, node, outSet'))
		    end

		val (livein', liveout') = List.foldr intNode (livein, liveout) (nodes control)
	  in
		if liveMapEq(livein', livein) andalso liveMapEq(liveout', liveout) 
		then (livein', liveout') 
		else  intGraph (livein', liveout') 
	  end
	
      val (livein, liveout) = intGraph (tigergraph.newTable(), tigergraph.newTable())
  in 
    (lIn := livein; lOut := liveout)
  end



(*

liveout

[Splaymap.find (m, k)] returns v if m maps k to v; otherwise raises NotFound.

*)
fun liveout node = Splayset.listItems (Splaymap.find (!lOut, node))

(*

printLiveOut

*)
fun printLiveOut fg =
let
    val (FGRAPH {control, def, use, ismove}) = fg

    fun printNode n = 
           (print (nodename n^"-> "); 
           List.map (fn t => print (t^",")) (liveout n); 
           print "\n") 
in 
    List.app printNode (nodes control)
end


(*

Grafo de interferencia. 

*)
val igraph = newGraph()

(*

Tablas : nodo a temp y de temp a nodo

*)
val nodeTab : (tigergraph.node, tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash, tigergraph.eq) (100, NotFound)  
val tempTab : (tigertemp.temp, tigergraph.node) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash, (fn (a,b) => (String.compare(a,b) = EQUAL))) (100, NotFound)  

(* 

tnode  

Actualiza igraph, tempTab, nodeTab

*)
fun tnode temp = case (Polyhash.peek tempTab temp) 
                 of SOME node => node
                  | NONE => (let 
				val n = newNode(igraph) 
			     in 
				Polyhash.insert tempTab (temp, n);
                                Polyhash.insert nodeTab (n, temp);
                                n 
                             end)

(*

gtemp 

*)
fun gtemp node = Polyhash.find nodeTab node

(* 

mk_edges

Agrega aristas dirigidas de un nodo a una lista de nodos 

*)
fun mk_edges x blist = List.app (fn y => mk_edge {from = tnode(x), to = tnode(y)}) blist

(* 

mk_iedges

Agrega las aristas 

*)
fun mk_iedges (alist, blist) = List.app (fn x => mk_edges x blist) alist

(*

movesref

*)
val movesref = ref ([] : ((tigergraph.node * tigergraph.node) List.list))

(*

interferenceGraph: tigerflow.flowgraph -> igraph * liveness

Takes a flow graph and returns an interference graph and a table mapping each
flow-graph node to the set of temps that are live-out at that node.

*)
fun interferenceGraph flowgraph =
let
      val (FGRAPH{control = fgraph, def, use, ismove}) = flowgraph
   
      (* Procesa un nodo del flowgraph. P225 *)
      fun instr_interf flowNode = 
          let
	        val ismove' = Splaymap.find (ismove, flowNode)  (* XXX bool *)
	        val def' = Splaymap.find (def, flowNode)        
	        val use' = Splaymap.find (use, flowNode)        (* XXX tigertemp.temp list *)
	  in
		if ismove' then (
		   mk_iedges(def', List.filter (fn x => x <> List.hd use') (liveout flowNode)); 
		   movesref := ((tnode(List.hd def'), tnode(List.hd use')) :: !movesref ))  
		else
                   (* *)
		   mk_iedges(def', liveout flowNode)
	  end
	 
	 
in        
	calcLiveness flowgraph;
        List.app instr_interf (nodes fgraph) ;

        (IGRAPH {graph = igraph,
	         tnode = tnode, 
	         gtemp = gtemp, 
	         moves = !movesref}, 
	         liveout)

end     

(*

show

*)
fun show (IGRAPH{graph, gtemp, ...}) = tigergraph.printGraphWithNaming graph gtemp


end
