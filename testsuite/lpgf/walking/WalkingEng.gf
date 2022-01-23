-- From Angelov, Bringert, Ranta (2009)
concrete WalkingEng of Walking = {
  lincat
    S = Str ;
    NP = {s : Str; n : Number} ;
    VP = {s : Number => Str} ;
  lin
    And s1 s2 = s1 ++ "and" ++ s2 ;
    Pred np vp = np.s ++ vp.s ! np.n ;
    John = {s = "John"; n = Sg} ;
    We = {s = "we"; n = Pl} ;
    Walk = {s = table {
        Sg => "walks";
        Pl => "walk"
      }
    } ;
  param
    Number = Sg | Pl ;
}
