concrete LiteralsCnc of Literals = open Prelude in {
  lincat S = SS ;
  lin
    mkString s = ss ("«" ++ s.s ++ "»") ;
    mkInt s = s ;
    mkFloat s = s ;

    and s1 s2 = ss (s1.s ++ "and" ++ s2.s) ;
}
