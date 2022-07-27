type nucleotide = A | C | G | T

let check_dna x y = if x == y then 0 else 1
let hamming_distance l1 l2 =
  match (List.length l1, List.length l2) with
  | (0, 0) -> Ok 0
  | (0, _) -> Error "left strand must not be empty"
  | (_, 0) -> Error "right strand must not be empty"
  | (a, b) when a <> b -> Error "left and right strands must be of equal length"
  | (_, _) -> Ok (List.map2 check_dna l1 l2 |> List.fold_left (+) 0) 
  
