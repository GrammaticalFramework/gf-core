concrete Params2Cnc of Params2 = {

  param
    Boolean = True | False;
    AForm = APred | AMod GenNum;
    GenNum = GSg Gender Boolean | GPl;
    Gender = Masc | Fem | Neutr;

  lincat
    Quality = {s : AForm => Str ; g : Gender};
    MassKind = {s : Str};

  lin
    SuchMassKind qual = {
      s = qual.s ! AMod (GSg qual.g True)
    };

    Good = {
      s =
        table {APred => "pred";
               AMod (GSg Masc True) => "mod sg masc t";
               AMod (GSg Fem True) => "mod sg fem t";
               AMod (GSg Neutr True) => "mod sg neutr t";
               AMod (GSg Masc False) => "mod sg masc f";
               AMod (GSg Fem False) => "mod sg fem f";
               AMod (GSg Neutr False) => "mod sg neutr f";
               AMod GPl => "mod pl"} ;
      g = Neutr
    };
}
