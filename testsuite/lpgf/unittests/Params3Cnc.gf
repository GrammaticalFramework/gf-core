concrete Params3Cnc of Params3 = {

  param
    Boolean = True | False;
    AForm = APred | AMod GenNum;
    GenNum = GSg Animacy Boolean | GPl;
    Animacy = Human Gender | Nonhuman ;
    Gender = Masc | Fem | Neutr;
  lincat
    S = Str ;
    G = { gen : Gender } ;
    T = AForm => Str ;
  lin
    mkPred = tbl ! APred ;
    mkModSgHumanTrue g = tbl ! AMod (GSg (Human g.gen) True) ;
    mkModSgHumanFalse g = tbl ! AMod (GSg (Human g.gen) False) ;
    mkModSgNonTrue = tbl ! AMod (GSg Nonhuman False) ;
    mkModSgNonFalse = tbl ! AMod (GSg Nonhuman False) ;
    mkModPl = tbl ! AMod GPl ;

    GMasc = { gen = Masc } ;
    GFem = { gen = Fem } ;
    GNeutr = { gen = Neutr } ;

  oper
    tbl = table {
      APred => "pred";
      AMod (GSg (Human Masc) True) => "mod sg human masc t";
      AMod (GSg (Human Masc) False) => "mod sg human masc f";
      AMod (GSg (Human Fem) True) => "mod sg human fem t";
      AMod (GSg (Human Fem) False) => "mod sg human fem f";
      AMod (GSg (Human Neutr) True) => "mod sg human neutr t";
      AMod (GSg (Human Neutr) False) => "mod sg human neutr f";
      AMod (GSg Nonhuman True) => "mod sg nonhuman t";
      AMod (GSg Nonhuman False) => "mod sg nonhuman f";
      AMod GPl => "mod pl"
    } ;
}
