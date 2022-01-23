concrete Params1Cnc of Params1 = {
  param
    P = Px | PRQ R Q | Py ;
    R = R0 | RT T ;
    T = T0 | T1 ;
    Q = Q3 | Q2 | Q1 ;
  lincat
    S = Str ;
    F = { r : R; q : Q } ;
  lin
    f1 = { r = R0 ; q = Q3 } ;
    f2 = { r = RT T1 ; q = Q1 } ;
    FtoS f = tbl ! PRQ f.r f.q ;
  oper
    tbl = table {
      Px => "Px" ;
      Py => "Py" ;
      PRQ R0 Q1 => "PRQ R0 Q1" ;
      PRQ R0 Q2 => "PRQ R0 Q2" ;
      -- PRQ R0 Q3 => "PRQ R0 Q3" ;
      PRQ (RT _) Q1 => "PRQ (RT _) Q1" ;
      -- PRQ (RT T0) Q1 => "PRQ (RT T0) Q1" ;
      PRQ (RT T0) Q2 => "PRQ (RT T0) Q2" ;
      -- PRQ (RT T0) Q3 => "PRQ (RT T0) Q3" ;
      -- PRQ (RT T1) Q1 => "PRQ (RT T1) Q1" ;
      PRQ (RT T1) Q2 => "PRQ (RT T1) Q2" ;
      -- PRQ (RT T1) Q3 => "PRQ (RT T1) Q3" ;
      PRQ _ Q3 => "PRQ _ Q3"
    } ;
}
