open Base
let acronym str = 
  String.map str ~f:(fun c -> match c with
  | '-' | '_'-> ' '
  | _ -> c)
  |>
  String.split ~on:' ' |>
  List.map ~f:(fun w -> 
    match String.to_list w with
    | [] -> ""
    | a :: _ -> Char.uppercase a |> Char.to_string
  ) 
  |> List.fold_left ~init:"" ~f:(fun acc s -> acc ^ s)
