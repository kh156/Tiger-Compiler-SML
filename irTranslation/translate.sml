signature TRANSLATE =
sig
  type level
  type access

  val outermost : level
  val newLevel : (parent: level, name: Temp.Label,
                  formals: bool list) -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> access
end

structure Translate : TRANSLATE =
struct
  type level = int
  type access = level * Frame.access

  structure Te = Temp
  structure Tr = Tree
  structure F = Frame

  val currLevel = ref 0

  val outermost: int = 0

  val allocLocal(l) = (!currLevel, F.allocLocal())

end