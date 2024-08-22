3 if 
    while 
        7 swap 1 - 
    ;
;

/' expected output:
Expression [Terminal (Val 3.0),If {ifTrue = Expression [While (Expression [Terminal (Val 7.0),Terminal (Word "swap"),Terminal (Val 1.0),Terminal (Word "-")])], ifFalse = Expression []}]

[7.0,7.0,7.0,0.0]
'/