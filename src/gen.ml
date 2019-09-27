module P = Presentation
module H = Ast_helper
module L = Lattice

let underscore ppf () = Format.fprintf ppf "_"


let nl = Location.mknoloc
let var name_map id =
  match L.M.find_opt id name_map with
  | Some x -> x
  | None ->
    Format.asprintf "%a" Pp.(list ~sep:underscore string) (L.Id.elements id)

let mid nm id = H.Mod.ident (nl @@ Longident.Lident (var nm id))


let ast nm pr =
  let final = L.level pr.P.id = 1 in
  let vb {L.expr; key;_} = [H.Vb.mk (H.Pat.var (nl key)) expr] in
  let item x = H.Str.value Asttypes.Nonrecursive (vb x) in
  let include' id = H.Str.include_  (H.Incl.mk (mid nm id)) in
  let str = H.Mod.structure @@
    (if final then List.map include' pr.froms else [])
    @ List.rev (L.Se.fold (fun x l -> item x :: l) pr.more [])
  in
  let mb = H.Mb.mk (nl @@ var nm pr.P.id) str in
  H.Str.module_ mb

let full nm prs =
  let sorted =
    List.sort (fun pry prx ->
        let rough = compare (L.level prx.P.id) (L.level pry.P.id) in
        if rough = 0 then compare prx pry else rough
      )
      prs in
  List.map (ast nm) sorted
