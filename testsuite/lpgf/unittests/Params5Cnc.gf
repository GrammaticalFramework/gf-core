concrete Params5Cnc of Params5 = {

  param
    P = P1 | PQ Q ;
    Q = Q1 | QR R ;
    R = R1 | R2 ;

  lincat
    P_ = P ;
    Q_ = Q ;
    R_ = R ;
    S = Str ;

  lin
    p1 = P1 ;
    pq q = PQ q ;
    q1 = Q1 ;
    qr r = QR r ;
    r1 = R1 ;
    r2 = R2 ;

    showP p = tblP ! p ;
    showPQ p q = tblPQRec ! { p = p ; q = q } ;
    showQP q p = tblQPRec ! { q = q ; p = p } ;

  oper
    tblP : P => Str ;
    tblP = table {
      P1 => "P1";
      PQ Q1 => "PQ Q1";
      PQ (QR R1) => "PQ (QR R1)";
      PQ (QR R2) => "PQ (QR R2)"
    } ;

    tblQPRec : {q:Q ; p:P} => Str ;
    tblQPRec = table {
      {q=Q1 ; p=P1} => "Q1 ; P1";
      {q=Q1 ; p=PQ Q1} => "Q1 ; PQ Q1";
      {q=Q1 ; p=PQ (QR R1)} => "Q1 ; PQ (QR R1)";
      {q=Q1 ; p=PQ (QR R2)} => "Q1 ; PQ (QR R2)";

      {q=QR R1 ; p=P1} => "QR R1 ; P1";
      {q=QR R1 ; p=PQ Q1} => "QR R1 ; PQ Q1";
      {q=QR R1 ; p=PQ (QR R1)} => "QR R1 ; PQ (QR R1)";
      {q=QR R1 ; p=PQ (QR R2)} => "QR R1 ; PQ (QR R2)";

      {q=QR R2 ; p=P1} => "QR R2 ; P1";
      {q=QR R2 ; p=PQ Q1} => "QR R2 ; PQ Q1";
      {q=QR R2 ; p=PQ (QR R1)} => "QR R2 ; PQ (QR R1)";
      {q=QR R2 ; p=PQ (QR R2)} => "QR R2 ; PQ (QR R2)"
    } ;

    tblPQRec : {p:P ;  q:Q} => Str ;
    tblPQRec = table {
      {p=P1 ; q=Q1} => "P1 ; Q1";
      {p=P1 ; q=QR R1} => "P1 ; QR R1";
      {p=P1 ; q=QR R2} => "P1 ; QR R2";

      {p=PQ Q1 ; q=Q1} => "PQ Q1 ; Q1";
      {p=PQ Q1 ; q=QR R1} => "PQ Q1 ; QR R1";
      {p=PQ Q1 ; q=QR R2} => "PQ Q1 ; QR R2";

      {p=PQ (QR R1) ; q=Q1} => "PQ (QR R1) ; Q1";
      {p=PQ (QR R1) ; q=QR R1} => "PQ (QR R1) ; QR R1";
      {p=PQ (QR R1) ; q=QR R2} => "PQ (QR R1) ; QR R2";

      {p=PQ (QR R2) ; q=Q1} => "PQ (QR R2) ; Q1";
      {p=PQ (QR R2) ; q=QR R1} => "PQ (QR R2) ; QR R1";
      {p=PQ (QR R2) ; q=QR R2} => "PQ (QR R2) ; QR R2"
    } ;

}
