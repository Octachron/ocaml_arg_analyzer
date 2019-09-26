let parse file =
  Pparse.parse_implementation ~tool_name:"arg_analyzer" file

module L = Longident

module Env = Map.Make(String)
let (.?()) m x = Env.find_opt x m


let rename env put lid = match lid with
  | L.Lident x ->
    begin match env.?(x) with
      | None -> put (L.Lident x)
      | Some y -> put (L.Lident y)
    end
  | any -> put any

module P = Parsetree

module A = Ast_mapper
let super = A.default_mapper

module Iter = Ast_iterator
let default_iter = Iter.default_iterator

let is_free v e =
  let t = ref true in
  let expr iter e =
    match e.P.pexp_desc with
    | Pexp_ident {txt=Lident x; _ } when x = v -> t:=false
    | _ -> default_iter.expr iter e in
  let iter = { default_iter with expr } in
  iter.expr iter e;
  !t

let rec expr counter env iter (e:P.expression) = match e.pexp_desc with
  | P.Pexp_ident lid -> rename env (fun txt -> { e with pexp_desc = Pexp_ident {lid with txt} }) lid.txt
  | P.Pexp_fun (Nolabel, None, pat, exp) ->
    let pat = iter.A.pat iter pat in
    fn e counter env iter pat exp
  | _ -> super.expr iter e
and fn super counter env iter pat e =
  let back pat expr = { super with pexp_desc =  P.Pexp_fun(Nolabel, None, pat, expr) } in
  match pat.P.ppat_desc with
  | Ppat_var ({txt= x; _ } as var) ->
    let id = string_of_int counter in
    let pat = { pat with ppat_desc = Ppat_var { var with txt=id } } in
    let env = Env.add x id env in
    let iter = { iter with expr = expr (counter+1) env } in
    let expr = iter.expr iter e in
    begin match expr.pexp_desc with
      | Pexp_apply(f,l) ->
        begin match List.rev l with
          | (_,x) :: l ->
            begin match x.P.pexp_desc with
              | Pexp_ident {txt=Lident t; _} when t = id ->
                if List.for_all (is_free id) (f :: List.map snd l) then
                  if l = [] then f else
                    { expr with pexp_desc = Pexp_apply(f, List.rev l) }
                else back pat expr
              | _ -> back pat expr
            end
          | [] -> back pat expr
        end
      | _ -> back pat expr
    end
  | Ppat_construct ({txt=Lident "()"; _ }, None) ->
    let expr = iter.expr iter e in
    begin match expr.pexp_desc with
      | Pexp_apply(f,l) ->
        begin match List.rev l with
          | (_,x) :: l ->
            begin match x.P.pexp_desc with
              | Pexp_construct ({txt=Lident "()"; _}, None) ->
                if l = [] then f else
                  { expr with pexp_desc = Pexp_apply(f, List.rev l) }
              | _ -> back pat expr
            end
          | [] -> back pat expr
        end
      | _ -> back pat expr
    end
  | _ -> back pat e

let normalize e =
  let env0 = Env.add "clear" "unset" Env.empty in
  let iter = { super with expr = expr 0 env0 } in
  iter.expr iter e


let bindings str =
  let bindings = ref [] in
  let store key expr =
    let norm = Format.asprintf "%a" Pprintast.expression (normalize expr) in
    bindings := (key,norm, expr) :: !bindings in
  let value_binding _ vb = match vb.P.pvb_pat.ppat_desc with
    | Ppat_var x when x.txt.[0] = '_' -> store x.txt vb.P.pvb_expr
    | _ -> () in
  let iter = { default_iter with value_binding } in
  iter.structure iter str;
  !bindings

let all f =
  let str = parse f in
  bindings str
