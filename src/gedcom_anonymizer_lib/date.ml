type t = {
  prefix : string;
  day : int;
  month : string;
  year : int
}

let get_day () = Random.int 27 + 1

let prefixes = [|""; "ABT"; "BEF"; "AFT"|]

let get_prefix () = prefixes.(Random.int 4)

let months = [|"JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN"; "JUL"; "AUG"; "SEP"; "OCT"; "NOV"; "DEC"|]

let get_month () = Array.get months (Random.int 12)

let get_year () = Random.int 1000 + 1900

let get_random_date () = {
  prefix = get_prefix ();
  day = get_day ();
  month = get_month ();
  year = get_year ();
}

let print_str fmt p =
  if p = "" then Format.fprintf fmt ""
  else Format.fprintf fmt "%s " p

let print_num fmt d =
  if d = 0 then Format.fprintf fmt ""
  else Format.fprintf fmt "%i " d

let to_string d =
  Format.asprintf "%a%a%a%a"
    print_str d.prefix
    print_num d.day
    print_str d.month
    print_num d.year
