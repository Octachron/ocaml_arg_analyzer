

let comma ppf () = Format.fprintf ppf ",@ "

let nl ppf () = Format.fprintf ppf "@,"

let space ppf () = Format.fprintf ppf " "


let list ~sep = Format.pp_print_list ~pp_sep:sep
let string = Format.pp_print_string

let text = list ~sep:space string

