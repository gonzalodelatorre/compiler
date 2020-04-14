signature tigerset =
sig
  
  type 'item set = 'item Splayset.set ref
  
  exception NotFound

  val newEmpty     : ('_item * '_item -> order) -> '_item set
  val add          : '_item set * '_item -> unit
  val addList      : '_item set * '_item list -> unit
  val member       : 'item set * 'item -> bool
  val delete       : '_item set * '_item -> unit
  val isEmpty      : 'item set -> bool
  val notEmpty     : 'item set -> bool
  val oneElem       : 'item set -> 'item 
  val union        : '_item set * '_item set -> '_item set 
  val intersection : '_item set * '_item set -> '_item set
  val difference   : '_item set * '_item set -> '_item set
  val app          : ('_item -> unit) -> '_item set -> unit
  val fold         : ('_item * 'b -> 'b) -> 'b -> '_item set -> 'b
  val all          : ('_item -> bool) -> '_item set -> bool
  val exists       : ('_item -> bool) -> '_item set -> bool
  val numItems     : ('item set) -> int
  val listItems    : ('item set) -> 'item list
  
end
