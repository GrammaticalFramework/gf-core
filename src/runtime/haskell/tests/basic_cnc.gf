concrete basic_cnc of basic = open Prelude in {

lincat N = {s : Str; is_zero : Bool} ;
lincat S = Str ;

printname fun z = "0" ;
printname fun s = "1" ;

lin z = {s="0"; is_zero=True} ;
    s n = {
      s = case n.is_zero of {
            True  => "1" ;
            False => n.s ++ "+" ++ "1"
          } ;
      is_zero = False
    } ;

lin c n = n.s ;

lincat P = {s:Str};
lin nat n = {s="nat"++SOFT_BIND++"("++SOFT_BIND++n.s++SOFT_BIND++")"};
lin ind pz ps n = {s=pz.s ++ "&" ++ "λ"++SOFT_BIND++ps.$0++SOFT_BIND++","++SOFT_BIND++ps.$1 ++ "." ++ ps.s} ;

lin intLit    n = n.s ;
lin stringLit s = s.s ;
lin floatLit  f = f.s ;

}
