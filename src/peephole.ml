open Util
open Pos
open Err
open Ast
open Tast
open Tast_util
open Tastmap

class peepholer m =
  object (visit)
    inherit Tastmap.tast_visitor m as super

    method expr_post (e, bty) =
      let e_ = e.data in 

      match e_ with
        | Classify (
          {data=IntLiteral _ | True | False} as e', _)
          ->
            (e', bty)
        | Select(cond, ({data=True}, {data=(Bool _)}), e2) 
          -> 
            let simplified = BinOp (LogicalOr, cond, e2) in
            ({data=simplified; pos=e.pos}, bty)
        | Select(cond, e1, ({data=False}, {data=(Bool _)})) 
          -> 
            let simplified = BinOp (LogicalAnd, cond, e1) in
            ({data=simplified; pos=e.pos}, bty)
        | Select(cond, 
          ({data=IntLiteral 1}, {data=(UInt(sz, lbl))}), 
          ({data=IntLiteral 0}, _)) (* select requires both arms to be of same type *)
          -> 
            let simplified = Bint ({data=Int (sz, lbl); pos=e.pos}, cond) in
            ({data=simplified; pos=e.pos}, bty)
        | BinOp(LogicalAnd, ({data=True}, {data=(Bool _)}), e')
        | BinOp(LogicalAnd, e', ({data=True}, {data=(Bool _)}))
          -> e'
        | _ -> (e, bty)

    method stm_post ({data; pos=p} as stm_) =
      match data with
        | Cmov (dest, cond, ({data=True}, ({data=(Bool s)} as bty)))
          -> [{data=Assign 
              (dest, 
                ({data=BinOp (LogicalOr, cond, 
                  ({data=Deref dest; pos=p}, {data=Bool s; pos=p})
                );pos=p}, bty)); 
              pos=stm_.pos
              }]
        (* | Cmov (dest, cond, ({data=False}, {data=(Bool _)}))
          -> [{data=Assign (dest, BinOp (LogicalAnd, UnOp(LogicalNot, cond), dest)); pos=stm_.pos}] *)
        | _ -> [stm_]
  end

let transform m =
  let visit = new peepholer m in
    visit#fact_module ()
