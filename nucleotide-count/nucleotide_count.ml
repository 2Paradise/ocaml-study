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

let count_nucleotides s = String.fold_result s ~init:empty ~f:(
  fun acc_map ch -> 
    if valid_dna_char ch
    then
      (
        let prev_cnt = 
          match Map.find acc_map ch with
          | Some cnt -> cnt
          | None -> 0 
        in
        
        Ok (Map.set acc_map ~key:ch ~data:(prev_cnt + 1))
      )
    else 
      Error ch
)
