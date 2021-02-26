concrete Params2Cnc of Params2 = {

  param
    AForm = APred | AMod GenNum;
    GenNum = GSg Gender | GPl;
    Gender = Masc | Fem | Neutr;

  lincat
    Quality = {s : AForm => Str ; g : Gender};
    MassKind = {s : Str};

  lin
    SuchMassKind qual = {
      s = qual.s ! AMod (GSg qual.g)
    };

    Good = {
      s =
        table {APred => "gut";
               AMod (GSg Masc) => "guter";
               AMod (GSg Fem) => "gute";
               AMod (GSg Neutr) => "gutes";
               AMod GPl => "gute"} ;
      g = Neutr
    };
}
