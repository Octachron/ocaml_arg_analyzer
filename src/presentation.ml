
module L = Lattice

type presentation = { id:L.Id.t; more: L.Se.t; froms: L.Id.t list }

let (.!()) = L.( (.!()) )

module M = L.M
module Se = L.Se

let too_small = 10

let topdown generator map id =
  let all = map.!(id).L.data in
  let potential_from = Lattice.down generator id in
  let candidates more =
    M.fold (fun id _ acc ->
        let x = map.!(id) in
        let diff = Se.diff more x.data in
        let w = Se.cardinal more - Se.cardinal diff in
        if w > too_small then (w,id,diff) :: acc else acc )
      potential_from [] in
  let candidate more =
    let sorted = List.sort (fun (x,_,_) (y,_,_) -> compare x y)
        (candidates more) in
    match sorted with
    | a :: _ -> Some a
    | _ -> None in
  let rec loop x =
    match candidate x.more with
    | None -> x
    | Some (_,id,more) ->
      loop { x with froms = id :: x.froms; more }
  in
  loop { id; more = all; froms = []}


let downtop generator map =
  let score_pair map candidate root =
    let data = map.!(candidate).L.data in
    let more = Se.diff root.more data in
    let diff = Se.cardinal root.more - Se.cardinal more in
    let score = float diff /. float (Se.cardinal data) in
    if diff > too_small
    then score, { root with more; froms = candidate :: root.froms }
    else 0., root in
  let score map candidate roots = List.fold_left ( fun (score, r) root ->
      let s, r' = score_pair map candidate root in
      s +. score, r' :: r
    ) (0.,[]) roots in
  let score_all map roots candidates =
    M.fold (fun candidate _ acc ->
        let score, roots = score map candidate roots in
        (score,roots,candidate):: acc ) candidates [] in
  let sort l = List.sort (fun (x,_,_) (y,_,_) -> compare y x) l in
  let update map candidates roots =
    match sort (score_all map roots candidates) with
    | (w, roots, c) :: _ -> Some (w,roots, c)
    | [] -> None in
  let next_candidates candidates =
    let union = M.union (fun _k l _ -> Some l) in
    M.fold (fun candidate _ acc -> union (L.up generator candidate) acc) candidates M.empty in
(*  let update_map c map =
    let base = map.!(c).L.data in
    M.map (fun {L.data;id} -> {L.id; data=Se.diff data base}) map in *)
  let rec loop map candidates active roots =
    match update map active roots with
    | Some (score, r, c) when score > 0.2 ->
      loop map candidates (M.remove c active) r
    | None | Some _ ->
      let next = next_candidates candidates in
      let next = M.filter (fun k _ -> L.level k > 1) next in
      if next = M.empty then roots else loop map next next roots in
  let start = M.add generator ("",generator) M.empty in
  let roots =
    List.map (fun id -> { id; more = map.!(id).L.data; froms = [] } )
      (List.map L.Id.singleton @@ L.Id.elements generator) in
  loop map start start roots

module IdSet = L.IdSet
module Id = L.Id

let flatten map prs =
  let all = List.fold_left (fun idset p ->
      List.fold_left (fun idset id -> IdSet.add id idset) idset (p.id :: p.froms)
    ) IdSet.empty prs in
  let final id =
    let more = map.!(id).L.data in
    let reduce x pr =
      if Id.subset pr.id x && not (Id.equal pr.id x) then
        { pr with more = Se.diff pr.more map.!(x).data; froms = x :: pr.froms }
      else pr in
    IdSet.fold reduce all { more; froms = []; id } in
  IdSet.fold (fun id m -> final id :: m) all []

let pp ppf pr =
  let plus ppf () = Format.fprintf ppf "+" in
  Format.fprintf ppf "%a@,@[%a+@]@,%a"
    L.pp_id pr.id (Pp.list ~sep:plus L.pp_id) pr.froms
    Extract.pp_binding (Se.elements pr.more)
