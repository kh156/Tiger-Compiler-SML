signature TRANSLATE = 
sig
	type level 
	type access (*not same as Frame.access*)

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access
	
end


structure Translate : TRANSLATE = 
  
  datatype level = ROOT
                  |CHILD of {parent: level, frame: F.frame}
  type access = level * Frame.access
  val outermost = ROOT

  fun newLevel {parent: level, name: label, formals: bool list} = 
    let 
      val formals' = true::formals
      val newframe = Frame.newFrame(label, formals')
    in
      CHILD(parent, newframe)
    end

  fun formals(level) = 
    let
      val theFrame = #frame level
      val theFormals = Frame.formals(theFrame)
    in
      case theFormals 
        of a::rest => rest
    end

  fun allocLocal(level) = 
    let 
      fun ret b:bool = 
        let
          val theFrame = #frame level
          val frameAccess = Frame.allocLocal theFrame b
        in
          (level, frameAccess)
        end
    in
      ret
    end

end
