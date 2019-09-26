
type entry =  Extract.entry = { key:string; normalized:string; expr:Parsetree.expression}

module Id = Set.Make(String)
module Se = Set.Make(struct
    type t = entry
    let compare x y = compare (x.key,x.normalized) (y.key,y.normalized) end)
module IdSet = Set.Make(Id)


type symb = Id.t
module M = Map.Make(Id)

type 'a node = { id: symb; data:'a }

let level = Id.cardinal

let down generator elt =
  let diff = Id.diff generator elt in
  let all = Id.fold (fun x -> M.add (Id.add x elt) (x,elt) ) diff M.empty in
  M.remove elt all


let pp_id ppf id =
  if id = Id.empty then
    Format.fprintf ppf "/"
  else
    Format.fprintf ppf "@[{%a}@]"
      Pp.(list ~sep:comma string)
      (Id.elements id)

let up generator elt =
  let add x = M.add (Id.remove x elt) (x,elt) in
  let r = M.remove elt @@ Id.fold add generator M.empty in
  r


let (.!()) m x = M.find x m

let create generator data =
  let add_descendant map parent atom id =
    let r = Se.inter map.!(parent).data map.!(Id.singleton atom).data in
    M.add id { id; data = r } map in
  let add_level map current =
    let f id (news,map) =
      let inf = down generator id in
      M.fold (fun id (atom, parent) (news, map) ->
          IdSet.add id news,
          add_descendant map parent atom id) inf (news,map)
    in
    IdSet.fold f current (IdSet.empty,map)
  in
  let rec loop map news =
    if news = IdSet.empty then map
    else
      let news, map = add_level map news in
      loop map news in
  let start = List.fold_left (fun map (name,se) ->
      let id = Id.singleton name in
      M.add id { id; data=se} map
    ) M.empty data in
  let first = IdSet.of_seq @@ Seq.map Id.singleton @@ Id.to_seq generator in
  loop start first




let pp_lattice_elt ppf {id;data} =
  Format.fprintf ppf "%a@,%a" pp_id id Extract.pp_binding (Se.elements data)


let pp_lattice ppf l =
  Format.fprintf ppf "@[<v>variant lattice:@,%a@]"
  Pp.(list ~sep:nl pp_lattice_elt)
  (List.map snd @@ M.bindings l)
