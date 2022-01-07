let pp_str fmt = Format.fprintf fmt "%s"

let pp_opt pp fmt = function
  | None -> Format.fprintf fmt ""
  | Some s -> Format.fprintf fmt " %a" pp s

let pp_id fmt i = Format.fprintf fmt "@%s@" i

let pp_gedcom_line fmt (lvl, id, tag, value) =
  Format.fprintf fmt
    "%i%a %s%a@."
    lvl
    (pp_opt pp_id) id
    tag
    (pp_opt pp_str) value
