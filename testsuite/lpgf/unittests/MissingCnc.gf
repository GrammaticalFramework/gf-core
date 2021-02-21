concrete MissingCnc of Missing = open Prelude, ParamX in {
  lincat
    S = SS ;
    NP = { s : Str ; n : Number } ;
    V2 = { s : Number => Str; s2 : Str } ;
  lin
    Pred subj verb obj = ss (subj.s ++ verb.s ! subj.n ++ obj.s ++ verb.s2) ;
    John = { s = "John" ; n = Sg } ;
    -- Dogs = { s = "dogs" ; n = Pl } ;
    Fish = { s = "fish" ; n = Pl } ;
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
}
