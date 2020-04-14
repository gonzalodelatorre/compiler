structure tigergraph :> tigergraph =
struct

  type node' = int
  type temp = tigertemp.temp

  datatype noderep = NODE of {succ: node' list, pred: node' list} 

  val emptyNode = NODE{succ=[], pred=[]}

  val bogusNode = NODE{succ=[~1], pred=[]}

  fun isBogus(NODE{succ= ~1::_, ...}) = true
    | isBogus _ = false

 
  structure A = struct open Dynarray
			
		 (* One-dimensional, mutable, zero-based unbounded arrays with elements of type noderep *)	  
	         type array = noderep array 
           
                end

  (* 

  graph y node

  *)
  type graph = A.array
  type node = graph * node'
  
  (*  

  eq: node*node -> bool 

  *)
  fun eq((_, a),(_, b)) = a = b
  

  (* 

  cmp: node*node -> order 

  *)
  fun cmp((_, a),(_, b)) = Int.compare(a, b)

  (* 

  augment: graph * node' -> node

  *)
  fun augment (g: graph) (n: node') : node = (g, n)

  (* 
     
   newGraph 

   Returns a dynamic array, all of whose elements are
   initialized to the default bogusNode

  *)
  fun newGraph() = A.array(0, bogusNode)

  (* 

  nodes: graph -> node list 

  *)
  fun nodes g = let 
		  (* Returns an upper bound on the indices *)
                  val b = A.bound g 
                  fun f i = if isBogus(A.sub(g, i)) then nil
			           else (g, i)::f(i+1)
		 in 
		   f 0			     
                 end

  (* 

  succ: node -> node list 

  *)
  fun succ(g, i) = let 
                    val NODE{succ=s, ...} = A.sub(g, i) 
		  in 
		    map (augment g) s 
		  end
  (* 

  pred: node -> node list 

  *)
  fun pred(g, i) = let 
		    val NODE{pred=p, ...} = A.sub(g, i)
                  in 
		    map (augment g) p 
		  end


  (*

  adj: node -> node list

  *)
  fun adj gi = pred gi @ succ gi



  (*

  newNode : graph -> node

  *)
  fun newNode g = (* binary search for unused node *)
    let 
      fun look(lo, hi) =
               (* i < lo indicates i in use
                  i >= hi indicates i not in use *)
            if lo = hi then (A.update(g, lo, emptyNode); (g, lo))
            else 
		 let 
		    val m = (lo+hi) div 2
                 in 
		    if isBogus(A.sub(g, m)) then look(lo, m) else look(m+1, hi)
                 end
    in 
      look(0, 1 + A.bound g)
    end


  (*


  *)
  exception GraphEdge



  (*

  delete: ( a * a list ) -> a list

  *)
  fun delete(i, j::rest) = if i = j then rest else j::delete(i, rest)
    | delete(_, nil) = raise GraphEdge


  (*

  diddle_edge

  *)
  fun diddle_edge change {from = (g:graph, i), to = (g':graph, j)} = 
      let 
          val NODE{succ = si, pred = pi} = A.sub(g, i)
          val _ = A.update(g, i, NODE{succ = change(j, si), pred = pi})
          val NODE{succ = sj, pred = pj} = A.sub(g, j)
          val _ = A.update(g, j, NODE{succ = sj, pred = change(i, pj)})
       in 
	 ()
      end


  (*

  mk_edge

  Create a directed edge

  *)
  val mk_edge = diddle_edge (op ::)

  (*

  rm_edge

  Delete an edge

  *)
  val rm_edge = diddle_edge delete


  (*


  *)
  type 'a table = (node, 'a) Splaymap.dict

  (*

  newTable

  [mkDict ordr] returns a new, empty map whose keys have ordering
   ordr.

  *)
   fun newTable () = Splaymap.mkDict cmp 
   

  (*

  nodename

  *)
   fun nodename(g, i:int) = "n" ^ Int.toString(i)


  (*

  printGraphWithNaming

  *)
   fun printGraphWithNaming g nodename = 
      let 
	  fun printNode n = (print (nodename n); print "\n" )
          
          fun printEdges n = List.app (fn x => (print (nodename n); print " --> "; print (nodename x); print "\n")) (succ n) 
      in  
        print "Nodos : \n";
        List.app printNode (nodes g);
        print "\n\n Aristas : \n";
        List.app printEdges (nodes g);
        print "\n\n\n"
      end
	
	fun printGraph g = printGraphWithNaming g nodename
end

