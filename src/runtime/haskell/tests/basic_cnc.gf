concrete basic_cnc of basic = open Prelude in {

lincat N = {s : Str; is_zero : Bool} ;
lincat S = Str ;

lin z = {s="0"; is_zero=True} ;
    s n = {
      s = case n.is_zero of {
            True  => "1" ;
            False => n.s ++ "+" ++ "1"
          } ;
      is_zero = False
    } ;

lin c n = n.s ;

lincat P = {};

}
