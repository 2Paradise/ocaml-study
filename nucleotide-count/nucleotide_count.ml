open Base
let empty = Map.empty (module Char)

let valid_dna_char = function
| 'A' | 'C' | 'G' | 'T' -> true
| _ -> false

let count_nucleotide s c =
  if valid_dna_char c 
  then
    match String.find s ~f:valid_dna_char with
    | Some valid_ch -> Ok (String.count s ~f:(Char.equal valid_ch))
    | None -> Error c
  else
    Error c
