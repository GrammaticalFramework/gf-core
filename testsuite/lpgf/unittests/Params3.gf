abstract Params3 = {
  cat G ; S ;
  fun
    mkPred : S ;
    mkModSgHumanTrue : G -> S ;
    mkModSgHumanFalse : G -> S ;
    mkModSgNonTrue : S ;
    mkModSgNonFalse : S ;
    mkModPl : S ;

    GMasc : G ;
    GFem : G ;
    GNeutr : G ;
}
