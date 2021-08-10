concrete MissingCnc of Missing = open Prelude, ParamX in {
  lincat
    S = SS ;
    Det = { s : Str ; n : Number } ;
    N = { s : Number => Str } ;
    NP = { s : Str ; n : Number } ;
    V2 = { s : Number => Str; s2 : Str } ;
    -- A = { s : Number => Str } ;
  lin
    Pred subj verb obj = ss (subj.s ++ verb.s ! subj.n ++ obj.s ++ verb.s2) ;

    ASg = { s = "a" ; n = Sg } ;
    -- APl = { s = "" ; n = Pl } ;
    -- TheSg = { s = "the" ; n = Sg } ;
    -- ThePl = { s = "the" ; n = Pl } ;

    Dog = { s = table { Sg => "dog" ; Pl => "dogs" } } ;
    -- Fish = { s = \\_ => "fish" } ;

    Love = {
      s = table {
        Sg => "loves" ;
        Pl => "love"
      } ;
      s2 = "a lot"
    } ;
    -- Eat = {
    --   s = table {
    --     Sg => "eats" ;
    --     Pl => "eat"
    --   } ;
    --   s2 = "often"
    -- } ;

    -- Big = { s = \\_ => "big" } ;
    -- Red = { s = \\_ => "red" } ;

    mkNP d n = { s = d.s ++ n.s ! d.n ; n = d.n } ;
    -- mkANP d a n = { s = d.s ++ a.s ! d.n ++ n.s ! d.n ; n = d.n } ;
}
