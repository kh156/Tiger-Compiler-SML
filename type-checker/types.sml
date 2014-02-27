structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
<<<<<<< HEAD
          | ARRAY of ty * unique
	  	  | NAME of Symbol.symbol * ty option ref
	  	  | UNIT
=======
          | ARRAY of ty * unique  
          | NAME of Symbol.symbol * ty option ref
	  	    | UNIT
	  	    | ERROR
>>>>>>> 2e18c536550cb013d56f1a25e7f133f36f6424c2

end

