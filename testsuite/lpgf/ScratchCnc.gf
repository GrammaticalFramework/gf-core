concrete ScratchCnc of Scratch = {
  param
    R = R1 | R2 ;
    P = PR R Q | PP ;
    Q = Q1 | Q2 ;
  lincat
    S = Str ;
    F = { p : P => Str } ;
  lin
    f1 = f2 ;
    f2 = { p = table {
      PR R1 Q1 => "R1 Q1" ;
      PR R1 Q2 => "R1 Q2" ;
      PR R2 _ => "R2 _" ;
      PP => "PP"
     } } ;
    FtoS f = f.p ! PR R1 Q2 ;
}
