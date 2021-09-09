open Util
open Pos
open Err
open Tast
open Llvm
open Pseudocode
open Printf
open Codegen

(* https://stackoverflow.com/questions/8373460/substring-check-in-ocaml *)
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

(* replace all instances of a in orig with b *)
let replace orig a b = 
  let re = Str.regexp_string a
  in Str.global_replace re b orig

class local_collector m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val mutable _vars : (var_name * base_type) list = []

    method _vars () =
      List.rev _vars

    (* method param param =
      begin
        match param.data with
          | Param (x,bty) ->
            _vars <- (x,bty) :: _vars
      end; super#param param 
      *)

    method block_only (block,next) =
      begin
        match block.data with
          | RangeFor (x,bty,_,_,_)
          | ArrayFor (x,bty,_,_) ->
            _vars <- (x,bty) :: _vars
          | _ -> ()
      end; super#block_only (block,next)

    method stm stm =
      begin
        match stm.data with
          | VarDec (x,bty,_)
          | FnCall (x,bty,_,_) ->
            _vars <- (x,bty) :: _vars
          | _ -> ()
      end; super#stm stm

  end

let collect_locals fdec =
  let m = Module([],[fdec],{fmap=[]}) in
  let visit = new local_collector m in
    visit#fact_module () |> ignore;
    visit#_vars ()

let sanitize_name s =
  s
  |> Str.global_replace (Str.quote "[" |> Str.regexp) "<"
  |> Str.global_replace (Str.quote "]" |> Str.regexp) ">"

let wasm_label str =
  "$" ^ sanitize_name str

let to_bool wexpr wbty =
  sprintf "(%s.ne %s (%s.const 0))" wbty wexpr wbty

let rec build_cast pos prefix oldsize newsize is_signed wasmexpr = 
  match (oldsize, newsize, is_signed) with
  (* TODO: cleanup this awful code *)
  | (8, 32, true) | (8, 64, true)
  | (16, 32, true) | (16, 64, true)
  | (32, 64, true) -> 
    sprintf "(%s%d.extend%d_s %s)" prefix newsize oldsize wasmexpr
  | (8, 64, false) | (16, 64, false) | (32, 64, false) ->
    sprintf "(%s%d.extend_i32_u %s)" prefix newsize wasmexpr
  | (64, 32, _) -> 
    sprintf "(%s32.wrap_%s64 %s)" prefix prefix wasmexpr
  | (64, 16, _) | (64, 8, _) -> 
    build_cast pos prefix 32 newsize is_signed 
      (build_cast pos prefix 64 32 is_signed wasmexpr) 
    (* first cast the thing from 64 to 32 bits, then cast it down again*)
  | (32, 16, _) -> 
    build_cast pos prefix 16 32 is_signed 
      (sprintf "(%s32.and (%s32.const 0xffff) %s)" prefix prefix wasmexpr)
    (* We may have to sign extend after truncation, so we cast it back to a 32-bit integer *)
  | (32, 8, _) | (16, 8, _) -> 
    build_cast pos prefix 8 32 is_signed 
      (sprintf "(%s32.and (%s32.const 0xff) %s)" prefix prefix wasmexpr)
    (* ditto *)
  | (8, 32, false) | (16, 32, false) 
  | (8, 8, _) | (16, 16, _) | (8, 16, false)
  | (32, 32, _) | (64, 64, _) -> wasmexpr (* no need to cast *)
  | (8, 16, true) ->
    build_cast pos prefix 8 32 is_signed wasmexpr
  | _ -> raise @@ cerr pos "unimp cast"

let append dest to_add : unit =
  dest.contents <- dest.contents ^ to_add;

class wasm (m: Tast.fact_module) = 
  object (visit)

  method sdec ({pos=p; data}: Tast.struct_type) : unit = 
    raise @@ cerr p "unimp sdec"

  method varname {pos=p;data} = 
    wasm_label data

  method is_secret {pos=p;data} = 
    match data with
    | Bool {data=Secret} 
    | UInt (_, {data=Secret})
    | Int (_, {data=Secret}) -> true
    | Bool {data=Public} 
    | UInt (_, {data=Public})
    | Int (_, {data=Public}) -> false
    | Ref (bty, _) -> visit#is_secret bty
    | _ -> raise @@ cerr p "unimp secret"
  
  method is_signed {pos=p;data} =
    match data with
    | UInt (_, _) -> false
    | Int (_, _) -> true
    | Ref (bty, _) -> visit#is_signed bty
    | _ -> raise @@ cerr p "unimp is_signed"
  
  method int_bitwidth {pos=p;data} = 
    match data with 
    | Bool _ -> 1
    | UInt (w, _) -> w
    | Int (w, _) -> w
    | Ref (bty, _) -> visit#int_bitwidth bty
    | _ -> raise @@ cerr p "unimp bitwidth %s" @@ show_base_type' data
  
  method bytesize {pos=p;data} =
    match data with
    | Bool _ -> 1
    | UInt (w, _) | Int (w, _) -> (w + 7) / 8
    | Ref (bty, _) -> visit#bytesize bty
    | _ -> raise @@ cerr p "unimp bytesize"

  method bty {pos=p;data} = 
    match data with
    | Bool {data=Secret} -> "s32"
    | Bool {data=Public} -> "i32"
    | UInt (32, {data=Secret}) | Int (32, {data=Secret})
    | UInt (8, {data=Secret})  | Int (8, {data=Secret})
    | UInt (16, {data=Secret}) | Int (16, {data=Secret}) -> "s32"
    | UInt (32, {data=Public}) | Int (32, {data=Public})
    | UInt (8, {data=Public})  | Int (8, {data=Public})
    | UInt (16, {data=Public}) | Int (16, {data=Public}) -> "i32"
    | UInt (64, {data=Secret}) | Int (64, {data=Secret}) -> "s64"
    | UInt (64, {data=Public}) | Int (64, {data=Public}) -> "i64"
    | Ref (bty, _) -> visit#bty bty
    | Arr (bty, _, _) -> "i32"
    | _ -> raise @@ cerr p "unimp bty %s" @@ show_base_type' data
  
  method arr_bty {pos=p;data} = 
    match data with
    | Arr (bty, _, _) -> bty
    | _ -> raise @@ cerr p "expected array type"

  method param {pos=p;data} = 
    match data with
      | Param (x, bty) -> 
        "(param " ^ (visit#varname x) ^ " " ^ (visit#bty bty) ^ ")"
        
  method fn_intrinsic fn args =
    let fname = fn.data in
    if (Str.string_match (Str.regexp "__\\([a-z]+\\)\\[\\([0-9]+\\)\\]_\\([a-z]+\\)_le") fname 0) then 
    begin
      let load_or_store = Str.matched_group 1 fname in
      let nbits = Str.matched_group 2 fname in
      let secret_or_public = Str.matched_group 3 fname in
      let wargs = List.map visit#expr args |> String.concat " " in
      match nbits with 
      | "32" | "64" -> begin
        match load_or_store with
          | "load" | "store" -> begin
            match secret_or_public with
            | "secret" -> 
              Some (sprintf "(s%s.%s 0 %s)" nbits load_or_store wargs)
            | "public" -> 
              Some (sprintf "(i%s.%s 1 %s)" nbits load_or_store wargs)
            | _ -> None
          end
          | _ -> None
        end
        | _ -> None
    end
    else if (Str.string_match (Str.regexp "__s?memzero\\[\\([0-9]+\\)\\]_\\([a-z]+\\)") fname 0) then
      let arr = List.nth args 0 in
      let (_, arr_ty) = arr in
      let Arr (bty, lexpr, _) = arr_ty.data in
      let len_in_words = visit#arr_len_in_words bty lexpr in
      let secret = visit#is_secret bty in
      let bzero = sprintf "bzero_%s" (if secret then "sec" else "pub") in

      Some (sprintf "(call %s %s %s)" (wasm_label bzero) (visit#expr arr) len_in_words)
    else None
  
  method rty rt = 
    match rt with
    | Some bty -> 
      "(result " ^ visit#bty bty ^ ")"
    | None -> ""

  method fsig rt params = 
    let params = List.map visit#param params |> String.concat " " in
    let ret = visit#rty rt in
    params ^ " " ^ ret
  
  method local l = 
    match l with
    | (name, bty) -> "(local " ^ (visit#varname name) ^ " " ^ (visit#bty bty) ^ ")"
  
  method binop p op is_signed we1 we2 bty =
    let sign = (if is_signed then "s" else "u") in
      match op with 
      | Ast.Plus -> sprintf "(%s.add %s %s)" bty we1 we2
      | Ast.Minus -> sprintf "(%s.sub %s %s)" bty we1 we2
      | Ast.Multiply -> sprintf "(%s.mul %s %s)" bty we1 we2
      | Ast.Divide -> 
        sprintf "(%s.div_%s %s %s)" bty sign we1 we2
      | Ast.Modulo -> 
        if is_signed then sprintf "(%s.rem_s %s %s)" bty we1 we2
        else sprintf "(%s.rem_u %s %s)" bty we1 we2
      | Ast.Equal -> sprintf "(%s.eq %s %s)" bty we1 we2
      | Ast.NEqual -> sprintf "(%s.ne %s %s)" bty we1 we2
      | Ast.GT -> 
        sprintf "(%s.gt_%s %s %s)" bty sign we1 we2
      | Ast.GTE -> 
        sprintf "(%s.ge_%s %s %s)" bty sign we1 we2
      | Ast.LT -> 
        sprintf "(%s.lt_%s %s %s)" bty sign we1 we2
      | Ast.LTE -> 
        sprintf "(%s.le_%s %s %s)" bty sign we1 we2
      | Ast.LogicalAnd -> 
        sprintf "(%s.and %s %s)" bty (to_bool we1 bty) (to_bool we2 bty)
      | Ast.BitwiseAnd ->
        sprintf "(%s.and %s %s)" bty we1 we2
      | Ast.LogicalOr -> 
        sprintf "(%s.or %s %s)" bty (to_bool we1 bty) (to_bool we2 bty)
      | Ast.BitwiseOr ->
        sprintf "(%s.or %s %s)" bty we1 we2
      | Ast.BitwiseXor -> sprintf "(%s.xor %s %s)" bty we1 we2
      | Ast.LeftShift -> sprintf "(%s.shl %s %s)" bty we1 we2
      | Ast.RightShift -> sprintf "(%s.shr_%s %s %s)" bty sign we1 we2
      | Ast.LeftRotate -> sprintf "(%s.rotl %s %s)" bty we1 we2
      | Ast.RightRotate -> sprintf "(%s.rotr %s %s)" bty we1 we2
  
  method unop p op wexpr wbty =
    match op with
    | Ast.LogicalNot -> 
      sprintf "(%s.eqz %s)" wbty wexpr
    | Ast.BitwiseNot -> 
      sprintf "(%s.xor %s (%s.const -1))" wbty wexpr wbty
    | Ast.Neg -> 
      sprintf "(%s.sub (%s.const 0) %s)" wbty wbty wexpr
    
  method lexpr {pos=p;data} = 
    match data with
    | LIntLiteral n -> sprintf "(i64.const %d)" n
    | LDynamic x -> sprintf "(get_local %s)" (wasm_label x.data)
    
  method addr_of arr lexpr =
    let (_, arr_ty) = arr in
    let arr_ty = visit#arr_bty arr_ty in (* get the underlying type of the array *)
    let elem_sz = visit#bytesize arr_ty in
    sprintf "(i32.add %s (i32.mul (i32.const %d) (i32.wrap_i64 %s)))"
    (visit#expr arr) elem_sz (visit#lexpr lexpr)

  method arr_len_in_words bty lexpr =
    sprintf "(i32.div_u (i32.add (i32.const 7) (i32.mul (i32.const %d) (i32.wrap_i64 %s))) (i32.const 8))"
      (visit#bytesize bty) (visit#lexpr lexpr)

  method expr ({pos=p; data}, bty) =
    let wbty = visit#bty bty in
    match data with
      | True -> "(" ^ wbty ^ ".const 1)"
      | False -> "(" ^ wbty ^ ".const 0)"
      | IntLiteral n -> sprintf "(%s.const %d)" wbty n
      | Variable x -> sprintf "(get_local %s)" (visit#varname x)
      | Enref e -> visit#expr e
      | Deref e -> visit#expr e
      | UnOp (op, e) -> 
        visit#unop p op (visit#expr e) wbty
      | BinOp (op, e1, e2) ->
        let we1 = visit#expr e1 in
        let we2 = visit#expr e2 in
        let e1ty = Tast_util.type_of e1 in
        let is_signed = Tast_util.(not (is_bool e1ty) && is_signed e1ty) in
          visit#binop p op is_signed we1 we2 (visit#bty e1ty)
      | Classify e -> sprintf "(%s.classify %s)" wbty (visit#expr e)
      | Declassify e -> sprintf "(%s.declassify %s)" wbty (visit#expr e)
      | TernOp (cond, e1, e2) | Select (cond, e1, e2) -> 
        let wcond = visit#expr cond in
        let we1 = visit#expr e1 in
        let we2 = visit#expr e2 in
        let (_, cond_bty) = cond in
        let op = (if (visit#is_secret cond_bty) then "sselect" else "select") in
        sprintf "(%s %s %s %s)" op we1 we2 wcond
      | Cast (castty, e) -> 
        let we = visit#expr e in
        let (_, oldbty) = e in
        let oldsize = visit#int_bitwidth oldbty in
        let newsize = visit#int_bitwidth castty in
        let is_signed = Tast_util.is_signed oldbty in
        let prefix = if (visit#is_secret castty) then "s" else "i" in
        build_cast p prefix oldsize newsize is_signed we
      | ArrayGet (e, lexpr) -> 
        let (_, arr_ty) = e in
        let arr_ty = visit#arr_bty arr_ty in (* the underlying type of the array *)
        let newsize = visit#int_bitwidth arr_ty in
        let oldsize = if (newsize <= 32) then 32 else newsize in
        let is_signed = visit#is_signed arr_ty in
        let prefix = if (visit#is_secret arr_ty) then "s" else "i" in
        let mem = if (visit#is_secret arr_ty) then "0" else "1" in
        let load = sprintf "(%s.load %s %s)"
          (visit#bty arr_ty) mem (visit#addr_of e lexpr) in
        build_cast p prefix oldsize newsize is_signed load
      | ArrayView (e, lexpr, _) -> (* we don't care about the length *)
        visit#addr_of e lexpr
      | ArrayCopy e -> 
        let res = ref "" in
        let dest_arr_ty = bty in
        let (src, src_arr_ty) = e in
        let Arr (dest_bty, dest_lexpr, _) = dest_arr_ty.data in
        let Arr (src_bty, _, _) = src_arr_ty.data in
        let len_in_words = visit#arr_len_in_words dest_bty dest_lexpr in
        let src_secret = visit#is_secret src_bty in
        let dest_secret = visit#is_secret dest_bty in
        let rsp = if dest_secret then "srsp" else "prsp" in
        let memcpy = sprintf "memcpy_%s_%s"
          (if src_secret then "sec" else "pub")
          (if dest_secret then "sec" else "pub") in
        
        append res @@ sprintf "(set_global %s (i32.sub (get_global %s) (i32.mul (i32.const 8) %s)))"
          (wasm_label rsp) (wasm_label rsp) len_in_words;
        append res @@ sprintf "(call %s %s (get_global %s) %s)" (wasm_label memcpy) (visit#expr e) (wasm_label rsp) len_in_words;
        append res @@ sprintf "(get_global %s)" (wasm_label rsp);

        res.contents
      | ArrayZeros lexpr -> 
        let res = ref "" in
        let arr_ty = bty in
        let Arr (bty, lexpr, _) = arr_ty.data in
        let len_in_words = visit#arr_len_in_words bty lexpr in
        let secret = visit#is_secret bty in
        let rsp = if secret then "srsp" else "prsp" in
        let bzero = sprintf "bzero_%s" (if secret then "sec" else "pub") in

        append res @@ sprintf "(set_global %s (i32.sub (get_global %s) (i32.mul (i32.const 8) %s)))"
          (wasm_label rsp) (wasm_label rsp) len_in_words;
        append res @@ sprintf "(call %s (get_global %s) %s)"
          (wasm_label bzero) (wasm_label rsp) len_in_words;
        append res @@ sprintf "(get_global %s)" (wasm_label rsp);

        res.contents
      | _ -> sprintf "(unimp %s)" (show_expr' data)

  method assignment p dest wexpr = 
    match dest with
      | ({data=Variable v}, _) -> 
        sprintf "(set_local %s %s)"
          (visit#varname v) wexpr
      | ({data=ArrayGet (e, lexpr)}, _) -> 
        let (_, arr_ty) = e in
        let arr_ty = visit#arr_bty arr_ty in (* the underlying type of the array *)
        let mem = if (visit#is_secret arr_ty) then "0" else "1" in
        let width = visit#int_bitwidth arr_ty in
        let suffix = if width < 32 then sprintf "%d" width else "" in
        sprintf "(%s.store%s %s %s %s)" 
          (visit#bty arr_ty) suffix mem (visit#addr_of e lexpr) wexpr
      | _ -> raise @@ cerr p "unimp assign"
    
  method fcall fn args = 
    let intrinsic = visit#fn_intrinsic fn args in
    let wargs = List.map visit#expr args |> String.concat " " in
    let fname = fn.data in
      begin
        match intrinsic with 
        | Some s -> s
        | None ->
          sprintf "(call %s %s)" 
            (wasm_label fname) wargs
      end

  method stm {pos=p; data} =
    match data with
      | VarDec (x, bty, e) -> 
        "(set_local " ^ (visit#varname x) ^ " " ^ visit#expr e ^ ")"
      | Assign (e1, e2) ->
        visit#assignment p e1 (visit#expr e2)
      | Cmov (e1, cond, e2) -> 
        begin
          let (_, cond_bty) = cond in
          let op = (if (visit#is_secret cond_bty) then "sselect" else "select") in
          visit#assignment p e1 (sprintf "(%s %s %s %s)" 
            op (visit#expr e2) (visit#expr e1) (visit#expr cond))
        end
      | FnCall (x, bty, fn, args) ->
        sprintf "(set_local %s %s)" 
          (visit#varname x) (visit#fcall fn args)
      | VoidFnCall (fn, args) -> (visit#fcall fn args)
      | Assume e -> ""

  method block ({pos=p; data}, next) = 
    let res = ref "" in
    begin
      match data with
        | Scope blk -> 
          append res (visit#block blk);
        | ListOfStuff stms -> 
          append res((List.map visit#stm stms) |> String.concat " ");
        | If (cond,thens,elses) ->
          append res (sprintf "(if %s (then %s) (else %s))" 
            (visit#expr cond)
            (visit#block thens)
            (visit#block elses)
          )
        | RangeFor (i, bty, e1, e2, blk) -> 
          let sign = (if (Tast_util.is_signed bty) then "s" else "u") in
          let iname = i.data in
          let wbty = visit#bty bty in
          let winame = wasm_label iname in
          append res (sprintf "(set_local %s %s) "
            (wasm_label iname) (visit#expr e1)
          );
          append res (sprintf 
            "(block (loop (br_if 1 (%s.ge_%s (get_local %s) %s)) %s (set_local %s (%s.add (get_local %s) (%s.const 1))) (br 0)))"
            wbty sign winame (visit#expr e2) (visit#block blk) winame wbty winame wbty
          )
        | _ -> append res (sprintf "(unimp %s)" (show_block' data))
    end;
    append res (visit#next next);
    res.contents

  method next {pos=p;data} =
    match data with
      | Block blk -> 
        visit#block blk
      | End -> ""
      | Return e -> 
        sprintf "%s (return %s)" 
          (visit#restore_rsps()) (visit#expr e)
      | VoidReturn -> 
        sprintf "%s (return)" (visit#restore_rsps())
  
  method init_rbps () = 
    "(local $srbp i32) (local $prbp i32) "
    ^ "(set_local $srbp (get_global $srsp)) "
    ^ "(set_local $prbp (get_global $prsp)) "
  
  method restore_rsps () =
      "(set_global $srsp (get_local $srbp)) "
    ^ "(set_global $prsp (get_local $prbp)) "

  method references_rsp body = 
    (contains body "$srsp") || (contains body "$prsp")

  method remove_rsp_bkp_rst body = 
    let no_init = replace body (visit#init_rbps()) "" in
    (replace no_init (visit#restore_rsps()) "")

  method fdec ({pos=p; data} as fdec) = 
    match data with
      | FunDec(name, fnattr, rt, params, body) -> 
        let res = ref "" in
        append res "(func ";
        append res (wasm_label name.data ^ " untrusted ");
        append res ((visit#fsig rt params) ^ " ");

        let vars = collect_locals fdec in
        append res ((List.map visit#local vars |> String.concat " ") ^ " ");

        let body = visit#block body in

        append res (visit#init_rbps());
        append res body;
        append res (visit#restore_rsps());

        append res ")";
        
        let res_without_rsp_bkp_rst = visit#remove_rsp_bkp_rst res.contents in
        (*
          quick and dirty optimization: if the body does not reference $prsp or $srsp (& thus cannot modify them), 
          then avoid generating backup & restore routines for them
        *)
        (* Printf.printf "test: %s\n" (res_without_rsp_bkp_rst); *)
        let references_rsp = visit#references_rsp res_without_rsp_bkp_rst in
        (* Printf.printf "references: %b\n" references_rsp; *)
        if(not references_rsp) then (
          res_without_rsp_bkp_rst
        ) else (
          res.contents
        )
      | StdlibFn(code, fnattr, rt, params) -> 
        begin
          match code with
          | SMemzero _ -> ""
          | Memzero _ -> ""
          | LoadLE _ -> ""
          | StoreLE _ -> ""
          | _ -> sprintf "(unimp %s)" (show_stdlib_code code)
        end
      | _ -> raise @@ cerr p "unimp fdec"
  
  method func_export {pos=p; data} =
    match data with
    | FunDec(name, fnattr, rt, params, body) ->
      let name = name.data in
      if fnattr.export then
        sprintf "(export \"%s\" (func %s))" name (wasm_label name)
      else
        ""
    | _ -> ""
  
  method fact_module () = 
    let res = ref "" in
    append res "(module ";
    let Module (sdecs,fdecs,minfo) = m in
    let _ = List.map visit#sdec sdecs in

    let memsz = 512 in
    let pagesz = 65536 in

    append res (sprintf "(import \"js\" \"secmem\" (memory $secmem secret %d)) " memsz);
    append res (sprintf "(import \"js\" \"pubmem\" (memory $pubmem %d)) " memsz);

    append res (sprintf "(global $srsp (mut i32) (i32.const %d)) " (memsz * pagesz));
    append res (sprintf "(global $prsp (mut i32) (i32.const %d)) " (memsz * pagesz));

    append res Wasm_intrinsics.memcpy_funcs;
    
    append res (List.map visit#fdec (List.rev fdecs) |> String.concat " ");

    append res (List.map visit#func_export (List.rev fdecs) |> String.concat " ");

    append res ")";
    res.contents
end

let codegen m = 
  let visit = new wasm m in
    visit#fact_module()