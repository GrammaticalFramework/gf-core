-- From Angelov, Bringert, Ranta (2009)
concrete ZeroGer of Zero = {
  lincat
    S = Str ;
    NP = {s : Str; n : Number; p : Person} ;
    VP = {s : Number => Person => Str} ;
  lin
    And s1 s2 = s1 ++ "und" ++ s2 ;
    Pred np vp = np.s ++ vp.s ! np.n ! np.p ;
    John = {s = "John"; n = Sg ; p = P3} ;
    We = {s = "wir"; n = Pl; p = P1} ;
    Walk = {s = table {
        Sg => table {
          P1 => "gehe" ;
          P2 => "gehst" ;
          P3 => "geht"
        } ;
        Pl => table {
          P1 => "gehen" ;
          P2 => "geht" ;
          P3 => "gehen"
        }
      }
    } ;
  param
    Number = Sg | Pl ;
    Person = P1 | P2 | P3 ;
}
