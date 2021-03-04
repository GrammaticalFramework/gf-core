abstract Params5 = {
  cat
    P_ ;
    Q_ ;
    R_ ;
    S ;

  fun
    p1 : P_ ;
    pq : Q_ -> P_ ;
    q1 : Q_ ;
    qr : R_ -> Q_ ;
    r1 : R_ ;
    r2 : R_ ;

    showP : P_ -> S ;
    showPQ : P_ -> Q_ -> S ;
    showQP : Q_ -> P_ -> S ;
}
