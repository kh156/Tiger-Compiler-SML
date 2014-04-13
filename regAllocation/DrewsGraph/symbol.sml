signature SYMBOL =
sig
  eqtype symbol
  val symbol:  string -> symbol
  val name:    symbol -> string
  val compare: symbol * symbol -> order
  structure Set : ORD_SET sharing type Set.Key.ord_key = symbol
  structure Map : ORD_MAP sharing type Map.Key.ord_key = symbol
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int


  exception Symbol
  val nextsym = ref 0
  fun compare((_,n1),(_,n2)) = Int.compare(n1,n2)
			       
  structure StringMap = SplayMapFn(struct 
				   type ord_key = string
				   val compare = String.compare
				   end)
  val symTable:int StringMap.map ref = ref StringMap.empty			

  fun symbol name =
      case StringMap.find(!symTable, name)
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
		      val () = nextsym := i+1
		      val nt = StringMap.insert(!symTable,name,i)
		      val () = symTable := nt
	           in 
		      (name,i)
		  end

  fun name(s,n) = s

  structure OrdKey =
  struct 
    type ord_key = symbol
    val compare = compare
  end
  
  structure Set = SplaySetFn(OrdKey)
  structure Map = SplayMapFn(OrdKey)
end
