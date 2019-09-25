

let targets = ref []

let anonymous x = targets := x :: !targets


let comma ppf () = Format.fprintf ppf ",@ "

let nl ppf () = Format.fprintf ppf "@,"

let pp ppf toks =
  let space ppf () = Format.fprintf ppf " " in
  let text = Format.(pp_print_list ~pp_sep:space pp_print_string) in
  let pp ppf (k,args,x) = Format.fprintf ppf "%s(%a)=(%a)" k text args text x in
  Format.fprintf ppf "@[<hv>%a@]" Format.(pp_print_list ~pp_sep:comma pp) toks


let parse file =
  let chan = open_in file in
  Lets.start Lex.word (Lexing.from_channel chan)

let rename =
  let s = function "clear" -> "unset" | s -> s in
  fun (k,a,def) -> k,a, List.map s def

let beta_reduce (k,a,def)=
  let rec elim_prefix a d = match a, d with
    | _ :: _ , _ :: ":=" :: _ -> List.rev a, List.rev d
    | x::a, y :: d when x = y -> elim_prefix a d
    | a , d -> List.rev a, List.rev d in
  let a, d = elim_prefix (List.rev a) (List.rev def) in
  k, a, d

let simplify = List.map (fun x -> beta_reduce (rename x))

let print x =
  let toks = parse x in
  Format.printf "@[<v>%s:@,%a@,@]" x pp toks

let args = []
let msg = "compare arguments"

type entry = string * string list * string list
module Id = Set.Make(String)
module Se = Set.Make(struct type t = entry let compare = compare end)
module IdSet = Set.Make(Id)


type symb = Id.t
module L = Map.Make(Id)

type 'a node = { id: symb; data:'a }

let inferior generator elt =
  let diff = Id.diff generator elt in
  let all = Id.fold (fun x -> L.add (Id.add x elt) (x,elt) ) diff L.empty in
  L.remove elt all

let (.!()) m x = L.find x m

let lattice generator data =
  let add_descendant map parent atom id =
    let r = Se.inter map.!(parent).data map.!(Id.singleton atom).data in
    L.add id { id; data = r } map in
  let add_level map current =
    let f id (news,map) =
      let inf = inferior generator id in
      L.fold (fun id (atom, parent) (news, map) ->
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
      L.add id { id; data=se} map
    ) L.empty data in
  let first = IdSet.of_seq @@ Seq.map Id.singleton @@ Id.to_seq generator in
  loop start first


let pp_id ppf id =
  if id = Id.empty then
    Format.fprintf ppf "/"
  else
    Format.fprintf ppf "@[{%a}@]"
      Format.(pp_print_list ~pp_sep:comma pp_print_string)
      (Id.elements id)

let pp_lattice_elt ppf {id;data} =
  Format.fprintf ppf "%a@,%a" pp_id id pp (Se.elements data)


let pp_lattice ppf l =
  Format.fprintf ppf "@[<v>variant lattice:@,%a@]"
  Format.(pp_print_list ~pp_sep:nl pp_lattice_elt)
  (List.map snd @@ L.bindings l)


type presentation = { id:Id.t; more: Se.t; froms: Id.t list }

let present generator map id =
  let all = map.!(id).data in
  let potential_from = inferior generator id in
  let candidates more =
    L.fold (fun id _ acc ->
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
    pp_id pr.id (Format.pp_print_list ~pp_sep:plus pp_id) pr.froms
    pp (Se.elements pr.more)

let () =
  Arg.parse [] anonymous msg;
  let targets = !targets in
  let data = List.map (fun name -> name, Se.of_list (simplify @@ parse name)) targets in
  let generator = Id.of_list @@ List.map fst data in
  let latt = lattice generator data in
  let prs = List.map (fun (x,_) -> present generator latt (Id.singleton x)) data in
  Format.printf "@[<v>%a@]@." (Format.pp_print_list ~pp_sep:nl pp_presentation) prs
