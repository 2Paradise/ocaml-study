let leap_year y = 
    let cal_mod a = y mod a in
    cal_mod 4 = 0 && cal_mod 100 <> 0 || cal_mod 400 = 0
