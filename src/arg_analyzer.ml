

let targets = ref []

let anonymous x = targets := x :: !targets


let print x =
  let toks = Extract.all x in
  Format.printf "@[<v>%s:@,%a@,@]" x Pp.binding toks

let args = []
let msg = "compare arguments"

module L = Lattice

let module_name name =
  String.capitalize_ascii @@ Filename.chop_extension @@ Filename.basename name

let rec repeat n ppf x = if n = 0 then () else
    Format.fprintf ppf "%s%a" x (repeat (n-1)) x

let () =
  Arg.parse [] anonymous msg;
  let targets = !targets in
  let data = List.map (fun name -> module_name name, L.Se.of_list (Extract.all name)) targets in
  let generator = L.Id.of_list @@ List.map fst data in
  let latt = L.create generator data in
(*  let prs = List.map (fun (x,_) -> Presentation.topdown generator latt (L.Id.singleton x)) data in *)
  let prs = Presentation.downtop generator latt in
  let line ppf () = Format.fprintf ppf "@,%a@," (repeat 80) "—" in
  Format.printf "@[<v>%a@]@." Pp.(list ~sep:line Presentation.pp) prs
