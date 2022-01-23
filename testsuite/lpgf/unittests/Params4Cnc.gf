concrete Params4Cnc of Params4 = {

  param
    P = P1 | P2 ;
    Q = Q1 | Q2 ;
    R = RP P | RPQ P Q | R0 ;
    X = XPQ P Q ;

  lincat
    P_ = P ;
    Q_ = Q ;
    S = Str ;

  lin
    p1 = P1 ;
    p2 = P2 ;
    q1 = Q1 ;
    q2 = Q2 ;

    pqrec p q = tblPQRec ! { p = p ; q = q } ;

    rp p = tblR ! RP p ;
    rpq p q = tblR ! RPQ p q ;
    r0 = tblR ! R0 ;

    xpq p q = tblX ! XPQ p q ;

  oper
    tblP : P => Str ;
    tblP = table {
      P1 => "P1";
      P2 => "P2"
    } ;

    tblPQRec : {p:P; q:Q} => Str ;
    tblPQRec = table {
      {p=P1; q=Q1} => "P1 ; Q1";
      {p=P1; q=Q2} => "P1 ; Q2";
      {p=P2; q=Q1} => "P2 ; Q1";
      {p=P2; q=Q2} => "P2 ; Q2"
    } ;

    tblR : R => Str ;
    tblR = table {
      RP P1 => "RP P1";
      RP P2 => "RP P2";
      RPQ P1 Q1 => "RPQ P1 Q1";
      RPQ P1 Q2 => "RPQ P1 Q2";
      RPQ P2 Q1 => "RPQ P2 Q1";
      RPQ P2 Q2 => "RPQ P2 Q2";
      R0 => "R0"
    } ;

    tblX : X => Str ;
    tblX = table {
      XPQ P1 Q1 => "XPQ P1 Q1";
      XPQ P1 Q2 => "XPQ P1 Q2";
      XPQ P2 Q1 => "XPQ P2 Q1";
      XPQ P2 Q2 => "XPQ P2 Q2"
    } ;
}
