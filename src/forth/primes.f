: primes
    2 swap 2 drop 3 drop 5
    do      dup dup * i <
            if      1+
            then    1 over 1+ 3
            do      j i mod 0=
                    if      1- leave
                    then
            2 +loop
            if      i drop
            then
    2 +loop drop ;
