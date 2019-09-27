

let targets = ref []

let anonymous x = targets := x :: !targets


let print x =
  let toks = Extract.all x in
  Format.printf "@[<v>%s:@,%a@,@]" x Extract.pp_binding toks

let args = []
let msg = "compare arguments"

module L = Lattice

let module_name name =
  String.capitalize_ascii @@ Filename.chop_extension @@ Filename.basename name

let rec repeat n ppf x = if n = 0 then () else
    Format.fprintf ppf "%s%a" x (repeat (n-1)) x
let line ppf () = Format.fprintf ppf "@,%a@," (repeat 80) "—"

let resume ppf prs =
  Format.fprintf ppf "@[<v>%a@]@." Pp.(list ~sep:line Presentation.pp) prs

let threshold =
  "-threshold", Arg.Int ( (:=) Presentation.too_small), "set the smallest size of a submodule contribution"


module P = Presentation

let score prs = List.fold_left (fun score pr -> score + L.Se.cardinal pr.P.more + List.length pr.P.froms + 2) 0 prs

let name_map =
  List.fold_left (fun m (k,x) -> L.M.add (L.Id.of_list k) x m) L.M.empty
    [
      ["Opttopmain";"Topmain"], "Toplevel";
      ["Topmain"; "Opttopmain"; "Odoc_args"; "Main"; "Optmain"], "Common";
      ["Opttopmain"; "Optmain"], "Native";
      ["Optmain"; "Main"], "Compiler";
      ["Main"; "Optmain"; "Opttopmain"; "Topmain"], "Core";
      ["Topmain";"Main"], "Bytecode";
      ["Main"; "Optmain"; "Odoc_args"], "HalfCompiler";
    ]

let () =
  Arg.parse [threshold] anonymous msg;
  let targets = !targets in
  let data = List.map (fun name -> module_name name, L.Se.of_list (Extract.all name)) targets in
  let generator = L.Id.of_list @@ List.map fst data in
  let latt = L.create generator data in
(*  let prs = List.map (fun (x,_) -> Presentation.topdown generator latt (L.Id.singleton x)) data in *)
  let prs = Presentation.downtop generator latt in
  let prs = Presentation.flatten latt prs in
  Format.printf "@[%a@]@." Pprintast.structure  (Gen.full name_map prs)
