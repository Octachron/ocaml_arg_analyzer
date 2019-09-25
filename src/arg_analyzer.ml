

let targets = ref []

let anonymous x = targets := x :: !targets


let print x =
  let toks = Extract.all x in
  Format.printf "@[<v>%s:@,%a@,@]" x Pp.binding toks

let args = []
let msg = "compare arguments"

module L = Lattice

type presentation = { id:L.Id.t; more: L.Se.t; froms: L.Id.t list }

let (.!()) = L.( (.!()) )

module M = L.M
module Se = L.Se

let present generator map id =
  let all = map.!(id).L.data in
  let potential_from = Lattice.inferior generator id in
  let candidates more =
    M.fold (fun id _ acc ->
        let x = map.!(id) in
        let diff = Se.diff more x.data in
        let w = Se.cardinal more - Se.cardinal diff in
        if w > 5 then (w,id,diff) :: acc else acc )
      potential_from [] in
  let candidate more =
    let sorted = List.sort (fun (x,_,_) (y,_,_) -> compare x y)
        (candidates more) in
    match sorted with
    | a :: _ -> Some a
    | [] -> None in
  let rec loop x =
    match candidate x.more with
    | None -> x
    | Some (_,id,more) ->
      loop { x with froms = id :: x.froms; more }
  in
  loop { id; more = all; froms = []}

let pp_presentation ppf pr =
  let plus ppf () = Format.fprintf ppf "+" in
  Format.fprintf ppf "%a@,@[%a+@]@,%a"
    L.pp_id pr.id (Format.pp_print_list ~pp_sep:plus L.pp_id) pr.froms
    Pp.binding (Se.elements pr.more)

let () =
  Arg.parse [] anonymous msg;
  let targets = !targets in
  let data = List.map (fun name -> Filename.basename name, Se.of_list (Extract.all name)) targets in
  let generator = L.Id.of_list @@ List.map fst data in
  let latt = L.create generator data in
  let prs = List.map (fun (x,_) -> present generator latt (L.Id.singleton x)) data in
  Format.printf "@[<v>%a@]@." Pp.(list ~sep:nl pp_presentation) prs
