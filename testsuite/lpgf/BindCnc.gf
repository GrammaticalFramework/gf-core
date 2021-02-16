concrete BindCnc of Bind = open Prelude in {
  lincat
    S = SS ;
  lin
    f1 = ss ("hello the" ++ BIND ++ "re") ;
    f2 = ss ("good" ++ SOFT_BIND ++ "bye") ;
    concat a b = ss (a.s ++ b.s) ;
    bind a b = ss (a.s ++ BIND ++ b.s) ;
    softbind a b = ss (a.s ++ SOFT_BIND ++ b.s) ;
}
