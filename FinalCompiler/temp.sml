(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
  
  type temp = int
  val temps = ref 100
  val labs = ref 0
  fun newtemp() = let val t = !temps in temps := t+1; t end

  fun resetTempCount() = (temps := 100; labs := 0) (*Special registers already assigned*)

  structure Table = IntMapTable(type key = temp
			  fun getInt n = n)

  type 'a table= 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val look = Table.look

  fun makestring t = "$t" ^ Int.toString t

  type label = Symbol.symbol

  local structure F = Format
        fun postinc x = let val i = !x in x := i+1; i end
   in
      fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
      val namedlabel = Symbol.symbol
  end

  val compare = Int.compare
  structure TempOrd =
    struct 
      type ord_key = temp
      val compare = compare
    end

  structure Set = SplaySetFn(TempOrd)
  structure Map = SplayMapFn(TempOrd)
  
end
