-- From Angelov, Bringert, Ranta (2009)
abstract Zero = {
  flags startcat = S ;
  cat
    S; NP; VP;
  fun
    And : S -> S -> S ;
    Pred : NP -> VP -> S ;
    John : NP ;
    We : NP ;
    Walk : VP ;
}
