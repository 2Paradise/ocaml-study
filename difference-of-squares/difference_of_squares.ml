let square n = n * n

let square_of_sum n = 
    let rec cal_s_of_sq acc n = if n < 1 then acc else cal_s_of_sq (n + acc) (n - 1) in
    square (cal_s_of_sq 0 n)

let sum_of_squares n =
    let rec cal_sq_of_s acc n = 
        if n < 1 then acc else cal_sq_of_s (square n + acc) (n - 1) in
    cal_sq_of_s 0 n

let difference_of_squares n =
    (square_of_sum n) - (sum_of_squares n)
