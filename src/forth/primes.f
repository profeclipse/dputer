: primes
    2 swap 2 . 3 . 5
    do      dup dup * i <
            if      1+
            then    1 over 1+ 3
            do      j i umod 0=
                    if      1- leave
                    then
            2 +loop
            if      i u.
            then
    2 +loop drop ;
