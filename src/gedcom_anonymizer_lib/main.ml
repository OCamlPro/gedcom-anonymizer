(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 Steven de Oliveira <de.oliveira.steven@gmail.com>  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let lorem =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed a felis eget \
   odio pulvinar consequat viverra eget lorem. Phasellus imperdiet justo eget \
   neque pellentesque, et dictum metus finibus. Etiam egestas ac tellus a \
   tristique. Nulla facilisi. Phasellus volutpat lectus eu justo placerat \
   cursus. Fusce congue placerat lorem ac facilisis. Etiam vehicula euismod \
   arcu. Donec fermentum justo vel enim condimentum tempus. Pellentesque quis \
   scelerisque urna, euismod interdum purus. Proin eget auctor turpis, non \
   lacinia ex. Nam ante dui, rutrum vestibulum pretium in, pellentesque non \
   dui.Fusce pharetra vitae lectus vel congue. Vivamus at nunc non neque \
   tempus pulvinar. Nullam vitae nunc gravida, congue urna id, aliquam velit. \
   Aliquam sed viverra nibh. Suspendisse potenti. Sed ultrices diam nisi, eu \
   pharetra nulla vestibulum id. Aenean fringilla, dui eu consectetur \
   efficitur, lectus orci fermentum lacus, vitae efficitur ipsum neque quis \
   ligula. Etiam dignissim accumsan neque, at ornare sem posuere eget. Vivamus \
   et mi venenatis, molestie augue non, auctor orci. Fusce euismod, ligula vel \
   tristique gravida, tellus turpis viverra libero, eget lacinia orci neque \
   vitae nisi. Nullam sapien nulla, hendrerit et metus vitae, tempor pulvinar \
   sapien. Nulla vel varius odio, sit amet volutpat ex. Curabitur eu lorem eu \
   diam pulvinar ultricies maximus vel nunc. Morbi eget eleifend libero, sit \
   amet aliquam lacus. Sed vel varius est. Morbi at elementum tellus, vitae \
   facilisis tellus. Mauris vitae lacus sit amet lorem ultricies mattis. \
   Pellentesque consequat consectetur quam eget lacinia. Suspendisse lacinia \
   nec leo sed ultrices. Donec nec urna finibus, pretium neque sit amet, \
   ultricies dolor. Ut imperdiet nibh massa. Nullam nec eros quis nulla \
   elementum mattis. In blandit turpis eget dui faucibus tristique. Quisque a \
   ante id libero consequat posuere. Vivamus lacinia nisl a felis fringilla \
   blandit. Nunc semper mi quis eleifend pharetra. Proin blandit in ante ut \
   rutrum. Sed erat ex, iaculis et aliquam in, sodales at tortor. Ut bibendum \
   non libero quis commodo. Sed sagittis, sapien vitae facilisis maximus, \
   dolor sem tempor neque, sed dignissim tortor risus vel arcu. Mauris sit \
   amet nisl in mi molestie dictum eget sed risus. Fusce at elit molestie, \
   mattis est eu, mollis sem. Donec non magna id tellus rutrum tincidunt. \
   Nullam vehicula euismod lectus, at cursus dui scelerisque vitae. Interdum \
   et malesuada fames ac ante ipsum primis in faucibus. Donec fringilla, \
   mauris vel pulvinar venenatis, leo orci hendrerit tortor, ac blandit augue \
   libero eu nisi. Suspendisse non lobortis purus, nec imperdiet massa. Nunc \
   vel eleifend sem. Donec posuere eget nisi eu fringilla. Sed gravida \
   vulputate cursus. Vestibulum congue venenatis lorem ac luctus. Nullam id \
   neque faucibus nisl sodales euismod vitae ut sem. Vivamus ligula sapien, \
   convallis sed lectus eget, tincidunt pulvinar nunc. Praesent facilisis \
   ullamcorper fringilla. Donec condimentum molestie facilisis. Sed nec eros \
   fringilla, sollicitudin ex a, volutpat odio. Sed ornare tincidunt nibh nec \
   rutrum. Suspendisse lobortis, nunc non consectetur vehicula, leo quam \
   vulputate metus, nec dictum elit lacus a tellus. Nulla lorem nunc, \
   vestibulum non libero eu, euismod porttitor orci. Proin ut egestas urna. Ut \
   a hendrerit est. Vestibulum eleifend dolor a ante euismod pretium. \
   Suspendisse ultrices eros iaculis nisi pretium, porta aliquet felis \
   efficitur."

let lorem_size = String.length lorem

let names =
  [| "Albert"; "Bernard"; "Cyrille"; "Daniel"; "Éric"; "François"; "Gérard";
     "Hervé"; "Isidore"; "Jacques"; "Kevin"; "Louis"; "Michel"; "Nicolas";
     "Octave"; "Philippe"; "Quentin"; "René"; "Sylvain"; "Thierry"; "Urbain";
     "Vincent"; "Wolfgang"; "Xavier"; "Yann"; "Zébulon";
     "Anne"; "Brigitte"; "Cécile"; "Denise"; "Emmanuelle"; "Fanny";
     "Geneviève"; "Hélène"; "Isabelle"; "Joëlle"; "Karine"; "Lise"; "Marie";
     "Noëlle"; "Odile"; "Patricia"; "Quitterie"; "Rosine"; "Sidonie";
     "Thérèse"; "Ursule"; "Vanessa"; "Wilfried"; "Xavière"; "Yvonne"; "Zoé" |]

let string_table = Hashtbl.create 101
let name_table = Hashtbl.create 101
let lastname_table = Hashtbl.create 101

(* Selects a random substring of the lorem string of size size *)
let loremize size =
  if size >= lorem_size
  then lorem
  else
    let offset = Random.int (lorem_size - size) in
    String.sub lorem offset size

(* Given a string of size 'r', returns a random
   substring of lorem of size 'r' *)
let loremize_str str =
  loremize (String.length str)

(* Names with the format fname/lname return fname, lname, None
   Names with the format fname/lname/id return fname, lname, Some id
 *)
let get_name_from_reference str =
  match String.split_on_char '/' str with
  | [fname; lname] -> fname, lname, None
  | [fname; lname; id] -> fname, lname, Some id
  | _ -> assert false

(* Loremizes a wiki text (keeping compatibility with references)
let randomize_wiki_text str =
  (* Splitting the text enclosed by "[[" and "]]" ; expects to be well
     parenthesized. If str starts with "[[", we add "" at the beginning
     of the list *)
  let strs =
    let regexp = Str.regexp "\\[\\[\\|\\]\\]" in
    let substrings = Str.split regexp str in
    if
      String.length str <= 1
      || String.get str 0 <> '['
      || String.get str 1 <> '['
    then substrings
    else "" :: substrings
  in
  let anon_strs =
    List.mapi
      (fun i str ->
         if i land 1 = 0
         then (* text *) loremize_str str
         else (* reference *)

           match String.split_on_char '/' str with
           | fname :: lname :: id
      )
      strs*)

let random_age i = string_of_int (Random.int ( 2 * int_of_string i))

let random_num rang = string_of_int (Random.int rang)

let pure_random_name () =
  let fn = names.(Random.int 52) in
  let ln = names.(Random.int 52) in
  Format.sprintf "%s /%s/" fn ln

let add_new_name table key =
  let name = names.(Random.int 52) in
  Hashtbl.add table key name;
  name

let random_name_from_name fname lname =
  let new_fname =
    match Hashtbl.find_opt name_table fname with
    | Some name -> name
    | None -> add_new_name name_table fname
  in
  let new_lname =
    match Hashtbl.find_opt lastname_table lname with
    | Some name -> name
    | None -> add_new_name lastname_table lname in
  Format.sprintf "%s /%s/" new_fname new_lname

(* If the name has the format FNAME FNAME2 ... /LNAME/, transforms it into
   the same name format. Otherwise, returns a random FNAME LNAME. *)
let random_name old_name =
  match List.rev (String.split_on_char ' ' old_name) with
  | lname :: fname when Str.string_match (Str.regexp "/.*/") lname 0 -> begin
      (* Name format: FNAME FNAME2 ... /LNAME/ *)
      match String.split_on_char '/' lname with
      | [""; real_lname; ""] -> random_name_from_name (String.concat " " (List.rev fname)) real_lname
      | _ -> assert false
    end
  | _ -> pure_random_name ()

let update_line ((lvl, xref, tag, _) : Gedcom.gedcom_line) content : Gedcom.gedcom_line =
  if content = "" then
    (lvl, xref, tag, None)
  else
    (lvl, xref, tag, Some content)

(* Takes a line and its associated value, returns
   the line with an updated value. *)
let _anon_line (line : Gedcom.gedcom_line) (v : string) =
  let new_val =
    if v = "" then ""
    else if String.get v 0 = '@' then v
    else
      match Gedcom.tag line with
      | "AGE" -> random_age v
      | "DATE" -> Date.(to_string @@ get_random_date ())

      | "CALN" -> random_num 1000

      | "FAMF" | "FILE" -> "file.txt"
      | "FORM" -> "txt"

      | "AUTH" | "GIVN" | "NAME" | "NICK"
      | "SURN" -> random_name v

      | "COPR" -> "Private"
      | "CORP" -> pure_random_name () ^ " Corp."
      | "CITY" | "CTRY" | "NATI" | "STAE"
      | "PLAC" -> pure_random_name () ^ " Land"
      | "EDUC" -> random_num 10 ^ " years"
      | "IDNO" -> random_num 10000
      | "LANG" -> pure_random_name () ^ "landish"

      | "LATI" ->
          let num = random_num 89 ^ "." ^ random_num 999 in
          if Random.bool () then "N" ^ num else "S" ^ num

      | "LONG" ->
          let num = random_num 179 ^ "." ^ random_num 999 in
          if Random.bool () then "W" ^ num else "E" ^ num

      | "MEDI" -> if Random.bool () then "Book" else "Unknown"

      | "NCHI" | "NMR" | "PAGE" | "REFN" | "RFN"
      | "RIN" -> random_num 42
      | "NPFX" -> "PhD"
      | "NFSX" -> "Jr."
      | "OCCU" -> pure_random_name () ^ "'s assistant"
      | "PEDI" -> "pedigree"
      | "PHON" -> "+1-111-111-1142"
      | "POST" -> random_num 1000
      | "PROP" -> "Possessions"
      | "RELA" -> "Unspecified"
      | "RELI" -> "Unknown religion"
      | "RESN" -> "PRIVATE"
      | "ROLE" -> "Participant"
      | "SPFX" -> "Le"
      | "SSN" -> random_num 100
      | "STAT" -> "Status"
      | "TEMP" -> "Lorem Ipsum"
      | "TIME" -> random_num 23 ^ ":" ^ random_num 59 ^ ":" ^ random_num 59
      | "TITL" -> "Anonymous Title"
      | "WWW" -> "https://www.ocamlpro.com"

      | "ABBR" | "ADDR" | "ADR1" | "ADR2"
      | "AGNC" | "CAUS" | "CONC"
      | "CONT" | "DSCR" | "EMAI" | "NOTE"
      | "PUBL" | "RESI" | "TEXT"
      | "ADOP" | "ALIA" | "ANCE" | "ANCI" | "ANUL"
      | "ASSO" | "BAPL" | "BAPM" | "BARM"
      | "BASM" | "BLES" | "BURI" | "CENS"
      | "CHAN" | "CHAR" | "CHIL" | "CHR"
      | "CHRA" | "CONF" | "CONL" | "CREM"
      | "DATA" | "DEAT" | "DESC" | "DESI"
      | "DEST" | "DIV" | "DIVF" | "EMIG"
      | "ENDL" | "ENGA" | "EVEN" | "FAM"
      | "FAMC" | "FAMS" | "FCOM" | "GEDC"
      | "GRAD" | "HEAD" | "HUSB" | "IMMI"
      | "INDI" | "MARB" | "MARR" | "MARS"
      | "NATU" | "OBJE" | "ORDI" | "ORDN"
      | "PROB" | "REPO"
      | "RETI" | "SEX" | "SLGC" | "SOUR"
      | "SUBM" | "SUBN" | "TRLR" | "VERS"
      | "WIFE" | "MAP" | "BIRT"
        -> loremize_str v

      | "TYPE" -> loremize_str v
      (* TYPE is set aside because may require additional treatment *)

      | "EMAIL" | "FSID" -> loremize_str v
      (*  Not a valid 5.5.1 field, but found in some gedcom file*)

      | "FACT" | "FAX" | "FONE" | "QUAY"
      | "ROMN" | "WILL" as t ->
          Format.eprintf "Unhandled tag %s@." t;
          loremize_str v

      | t ->
          Format.eprintf "Unknown tag %s" t;
          loremize_str v

  in update_line line new_val

let anon_line l =
  let v = Gedcom.value l in
  match Hashtbl.find_opt string_table v with
  | None ->
      let res = _anon_line l v in
      Hashtbl.add string_table v res;
      res
  | Some res -> res


let anon_file input output =
  let inchan = open_in input in
  let outchan = open_out output in
  let fmt = Format.formatter_of_out_channel outchan in
  let lines = Gedcom.parse_lines inchan in
  List.iter
    (fun l -> Gedcom_print.pp_gedcom_line fmt (anon_line l))
    lines
