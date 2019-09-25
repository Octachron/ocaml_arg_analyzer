

let comma ppf () = Format.fprintf ppf ",@ "

let nl ppf () = Format.fprintf ppf "@,"

let space ppf () = Format.fprintf ppf " "


let list ~sep = Format.pp_print_list ~pp_sep:sep
let string = Format.pp_print_string

let text = list ~sep:space string

let binding ppf toks =
  let pp ppf (k,norm,_) = Format.fprintf ppf "%s=%s" k norm in
  Format.fprintf ppf "@[<hv>%a@]" (list ~sep:comma pp) toks
