abstract Missing = {
  cat S; Det; N; NP; V2; -- A;
  fun
    Pred: NP -> V2 -> NP -> S;
    ASg, {- APl, TheSg, -} ThePl : Det ;
    Dog, Fish: N ;
    Love, Eat: V2 ;
    -- Big, Red: A ;

    mkNP : Det -> N -> NP ;
    -- mkANP : Det -> A -> N -> NP ;
}
