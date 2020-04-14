structure tigerassem :> tigerassem = 
struct
    
    type reg = string
    type temp = tigertemp.temp
    type label = tigertemp.label

    (* Data type for assembly languege instruction wothout register assignments. Pag 201 *)
    datatype instr = OPER of {  assem: string,
				dst: temp list,
				src: temp list,
				jump: label list option }
		   | LABEL of { assem: string,
				lab: label  }
		   | MOVE of {  assem: string, 
				dst: temp, 
				src: temp } 

    (*

    format1

    *)
    fun format1 _ [] _ = ""
      | format1 m (#"'"::(c::(i::st))) (dst, src, jump) = 
            let val ll = case c of 
                     #"s" => src
                    | #"d" => dst
                    | _ => raise Fail ("Error -- no deberia pasar (format1)\n")
                val index = (ord i) - (ord #"0")
            in 
                (m (List.nth(ll, index)))^(format1 m st (dst, src, jump))
            end
     | format1 m (c::st) (d,s,j) = 
           (String.str c) ^ (format1 m st (d, s, j))


    (*
 
     format : ( string -> string ) -> instr -> string

     Formats an assembly instruction as a string. m is a function that shows the register assigment of every temp. Pag 201
 		  
    *)
    fun format m (OPER {assem, dst, src, jump}) = format1 m (String.explode assem) (dst, src, jump)
      | format m (MOVE {assem, dst, src}) = format1 m (String.explode assem) ([dst], [src], NONE)
      | format m (LABEL {assem, lab}) = (m lab)^":\n" 
    

end
