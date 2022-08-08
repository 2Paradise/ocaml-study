type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
let allergen_to_int = function
| Eggs -> 1
| Peanuts -> 2
| Shellfish -> 4
| Strawberries -> 8
| Tomatoes -> 16
| Chocolate -> 32
| Pollen -> 64
| Cats -> 128

let allergic_to i allergen =
    i land allergen_to_int allergen <> 0

let allergies i =
    List.filter (allergic_to i) [
    Eggs
    ; Peanuts
    ; Shellfish
    ; Strawberries
    ; Tomatoes
    ; Chocolate
    ; Pollen
    ; Cats
    ]

