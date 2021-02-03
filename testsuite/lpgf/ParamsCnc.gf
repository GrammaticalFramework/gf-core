concrete ParamsCnc of Params = {
  param
    R = R1 | R2 ;
    P = PR R Q | PP ;
    Q = Q1 | Q2 ;
  lincat
    S = Str ;
    F = { r : R } ;
  lin
    f1 = { r = R1 } ;
    f2 = { r = R2 } ;
    FtoS f = tbl ! PR f.r Q2 ;
  oper
    tbl = table {
      PR R1 Q1 => "PR R1 Q1" ;
      PR R1 Q2 => "PR R1 Q2" ;
      PR R2 _ => "PR R2 _" ;
      PP => "PP"
    } ;
}
