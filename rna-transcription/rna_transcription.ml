type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna list =
    List.map (fun dna -> match dna with
    | `G -> `C
    | `C -> `G
    | `T -> `A
    | `A -> `U
    ) list
