signature FINDESCAPE =
sig 
  val findEscape: Absyn.exp -> unit
end

structure FindEscape:FINDESCAPE =

struct
  structure A = Absyn
  structure S = Symbol

  type depth = int
  type escEnv = (depth * bool ref) S.table

  fun traverseVar(env:escEnv, d:depth, s:A.var): unit =
    case s of
      A.SimpleVar(symbol, pos) =>
        (case S.look(env, symbol) of
          SOME(depth, esc) => 
            if depth < d then esc:=true else ()
          | _ => ())
      | A.FieldVar(var,sym,pos) => traverseVar(env, d, var)    (********** Need escape record field????? **********)
      | A.SubscriptVar(var, exp, pos) => (traverseVar(env, d, var); traverseExp(env, d, exp))

  and traverseExp(env:escEnv, d:depth, s:A.exp): unit =
    let
      fun traverseExpShort(s:A.exp) = traverseExp(env, d, s)
    in
      case s of
        A.VarExp var => traverseVar(env, d, var)
        | A.NilExp => ()
        | A.IntExp e => ()
        | A.StringExp e => ()
        | A.CallExp {func, args: A.exp list, pos} =>
            app traverseExpShort args
        | A.OpExp {left, oper, right, pos} =>
            (traverseExpShort left; traverseExpShort right)
        | A.RecordExp {fields: (A.symbol * A.exp * A.pos) list, typ, pos} =>
            app traverseExpShort (map #2 fields)
        | A.SeqExp seqs =>
            app traverseExpShort (map #1 seqs)
        | A.AssignExp {var, exp, pos} =>
            (traverseVar(env, d, var); traverseExpShort exp)
        | A.IfExp {test, then', else', pos} =>
            (traverseExpShort test; traverseExpShort then';
              case else' of
                SOME exp => traverseExpShort exp
                | _ => ())
        | A.WhileExp {test, body, pos} =>
            (traverseExpShort test; traverseExpShort body)
        | A.ForExp {var, escape: bool ref, lo, hi, body, pos} =>
            let
              val env' = (escape := false; (** otherwise always true **) S.enter (env, var, (d, escape)))
            in
              (traverseExp(env', d, lo);
                traverseExp(env', d, hi);
                traverseExp(env', d, body))
            end
        | A.BreakExp e => ()
        | A.LetExp {decs, body, pos} =>
            let
              val env' = traverseDecs(env, d, decs)
            in
              traverseExp(env', d, body)
            end
        | A.ArrayExp {typ, size, init, pos} =>
            (traverseExpShort size; traverseExpShort init)
    end


  and traverseDecs(env, d, s:A.dec list): escEnv =
    let
      fun 
        traverseDecWrap (A.FunctionDec fundecList, env) =
          let
            fun addParam ({name, escape, typ, pos}, env) =
              (escape := false;   (** otherwise always true **)
                S.enter(env, name, (d+1, escape)))
            fun traverseDec({name, params, result, body:A.exp, pos}) = 
              let
                val env' = foldl addParam env params
              in
                traverseExp(env', d+1, body)
              end
          in
            (app traverseDec fundecList; env)
          end
        | traverseDecWrap (A.VarDec {name, escape, typ, init, pos}, env) =
          (escape := false; (** otherwise always true **)
            S.enter(env, name, (d, escape
              )))
        | traverseDecWrap (A.TypeDec typeDecList, env) = env      (********** Need traverse record type????? **********)
        | traverseDecWrap (_, env) = env
    in
      foldl traverseDecWrap env (rev s)
    end


  fun findEscape(prog: A.exp): unit =
    traverseExp(S.empty, 0, prog)
end