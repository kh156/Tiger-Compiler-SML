structure FindEscape: 

sig 
	val findEscape: Absyn.exp -> unit 
end

= struct
  structure A = Assem
  structure S = Symbol

  type depth = int
  type escEnv = (depth * bool ref) S.table

  fun traverserVar(env:escEnv, d:depth, s:A.var): unit =
    case s of
      A.SimpleVar(symbol, pos) =>
        case S.look(env, symbol) of
          SOME(depth, esc) => 
            if depth < d then esc:=true else ()
          | _ => ()
      | A.FieldVar(var,sym,pos) => traverseVar(env, d, var)    (********** Need escape record field????? **********)
      | A.SubscriptVar(var, exp, pos) => (traverseVar(env, d, var); traverseExp(env, d, exp))


  and traverseExp(env:escEnv, d:depth, s:A.exp): unit =
    let
      val traverseExpShort(s:A.exp) = traverseExp(env, d, s)
    in
      case A.exp of
        A.VarExp var => traverseExpShort var
        | A.NilExp e => ()
        | A.IntExp e => ()
        | A.StringExp e => ()
        | A.CallExp {func: symbol, args: exp list, pos: pos} =>
            app traverseExpShort args
        | A.OpExp {left: exp, oper: oper, right: exp, pos: pos} =>
            (traverseExpShort left; traverseExpShort right)
        | A.RecordExp {fields: (symbol * exp * pos) list, typ: symbol, pos: pos} =>
            app traverseExpShort (map #2 fields)
        | A.SeqExp seqs =>
            app traverseExpShort (map #1 seqs)
        | A.AssignExp {var: var, exp: exp, pos: pos} =>
            (traverserVar(env, d, var); traverseExpShort exp)
        | A.IfExp {test: exp, then': exp, else': exp option, pos: pos} =>
            (traverseExpShort test; traverseExpShort then';
              case else' of
                SOME exp => traverseExpShort exp
                | _ => ())
        | A.WhileExp {test: exp, body: exp, pos: pos} =>
            (traverseExpShort test; traverseExpShort body)
        | A.ForExp {var: symbol, escape: bool ref, lo: exp, hi: exp, body: exp, pos: pos} =>
            let
              () = escape := false; (** otherwise always true **)
              val env' = S.enter (env, var, (d, escape))
            in
              (traverseExp(env', d, lo);
                traverseExp(env', d, hi);
                traverseExp(env', d, body))
            end
        | A.BreakExp e => ()
        | A.LetExp {decs: dec list, body: exp, pos: pos} =>
            let
              val env' = traverseDecs(env, d, decs)
            in
              traverseExp(env', d, body)
            end
        | A.ArrayExp {typ: symbol, size: exp, init: exp, pos: pos} =>
            (traverseExpShort size; traverseExpShort init)
    end


  and traverseDecs(env, d, s:A.dec list): escEnv =
      case s of
        A.FunctionDec fundecList =>
          let
            fun addParam ({name, escape, typ, pos}, env) =
              (escape := false;   (** otherwise always true **)
                S.enter(env, name, (escape, d+1)))
            fun traverseDec({name, params, result, body, pos}, env) = 
              let
                val env' = foldr addParam evn params
              in
                traverseExp(env', d+1, body)
              end
          in
            (app traverseDec FunctionDec; env)
          end
      | A.VarDec {name, escape, typ, init, pos} =>
        (escape := false; (** otherwise always true **)
          S.enter(env, name, (escape, d)))
      | A.TypeDec typeDecList => env      (********** Need traverse record type????? **********)


  fun findEscape(prog: A.exp): unit =
    traverseExp(S.empty, 0, prog)
end