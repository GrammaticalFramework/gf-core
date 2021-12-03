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

lincat P = {};

}
