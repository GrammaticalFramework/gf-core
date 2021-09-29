resource param_table = {

param Q = Q1 | Q2 ;
param P = P1 | P2 Q ;

oper ab_patt = #["ab"];

oper test : Str -> Q = \s ->
  case s of {
    #ab_patt + _ => Q1 ;
    _            => Q2
  } ;

}
