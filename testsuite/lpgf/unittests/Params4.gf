abstract Params4 = {
  cat
    P_ ;
    Q_ ;
    S ;

  fun
    p1 : P_ ;
    p2 : P_ ;
    q1 : Q_ ;
    q2 : Q_ ;

    pqrec : P_ -> Q_ -> S ;

    rp : P_ -> S ;
    rpq : P_ -> Q_ -> S ;
    r0 : S ;

    xpq : P_ -> Q_ -> S ;
}
