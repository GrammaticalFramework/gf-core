concrete TablesCnc of Tables = {
  param
    R = R1 | R2 ;
    Q = Q3 | Q2 | Q1 ;
  oper
    ParamRec: Type = { r: R; q: Q } ;
  lincat
    S = Str ;
    F = { pr : ParamRec } ;
  lin
    f1 = { pr = { r = R1; q = Q1 } } ;
    f2 = { pr = { r = R1; q = Q2 } } ;
    f3 = { pr = { r = R1; q = Q3 } } ;
    f4 = { pr = { r = R2; q = Q1 } } ;
    f5 = { pr = { r = R2; q = Q2 } } ;
    f6 = { pr = { r = R2; q = Q3 } } ;

    FtoS f = tbl ! f.pr ;
    FtoS2 f = tbl ! { r = R2 ; q = f.pr.q } ;
  oper
    tbl = table {
      { r = R1 ; q = _ }  => "R1 _" ;
      { r = _  ; q = Q2 } => "_ Q2" ;
      { r = R2 ; q = Q3 } => "R2 Q3" ;
      _ => "_ _"
    } ;
}
