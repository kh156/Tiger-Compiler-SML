signature TEMP = 
sig
  eqtype temp
  val newtemp : unit -> temp
  structure Table : TABLE sharing type Table.key = temp
  val makestring: temp -> string
  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
  val resetTempCount : unit -> unit

  val compare : temp * temp -> order
  structure TempOrd : ORD_KEY sharing type TempOrd.ord_key = temp
  structure Set : ORD_SET sharing type Set.Key.ord_key = temp
  structure Map : ORD_MAP sharing type Map.Key.ord_key = temp

  type 'a table
  val empty : 'a table
  val enter : 'a table * temp * 'a -> 'a table
  val look  : 'a table * temp -> 'a option
end
