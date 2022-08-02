
let list_rainInfo = [(3,"Pling");(5,"Plang");(7,"Plong")]
let raindrop num =
    let res = List.fold_left (
        fun acc (i, str) -> 
            if num mod i = 0 then acc ^ str else acc
    ) "" list_rainInfo 
    in
    match res with
    | "" -> string_of_int num
    | r -> r

