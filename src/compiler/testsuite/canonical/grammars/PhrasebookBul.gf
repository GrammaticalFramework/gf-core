--# -path=.:present

concrete PhrasebookBul of Phrasebook =
  open
    SyntaxBul,
    (R = ResBul),
    ParadigmsBul,
    Prelude in {

  lincat
    PlaceKind = CNPlace ;

  oper
    CNPlace : Type = {name : CN ; at : Prep ; to : Prep; isPl : Bool} ;

    mkPlace : N -> Prep -> {name : CN ; at : Prep ; to : Prep; isPl : Bool} = \n,p ->
      mkCNPlace (mkCN n) p to_Prep ;

    mkCNPlace : CN -> Prep -> Prep -> CNPlace = \p,i,t -> {
      name = p ;
      at = i ;
      to = t ;
      isPl = False
      } ;

    na_Prep = mkPrep "на" R.Acc ;

  lin
    Airport = mkPlace (mkN066 "летище") na_Prep ;

}
