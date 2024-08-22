1 while             /' [1] '/
    2 while         /' [1 2] '/
        1 -         /' [1 7 1] '/
        7 swap      
    ;
    /' [1 7 7 0] '/
;

/' expected output: 
Expression [Terminal (Val 1.0),While (Expression [Terminal (Val 2.0),While (Expression [Terminal (Val 1.0),Terminal (Word "-"),Terminal (Val 7.0),Terminal (Word "swap")])])]

[1.0,7.0,7.0,0.0]
'/