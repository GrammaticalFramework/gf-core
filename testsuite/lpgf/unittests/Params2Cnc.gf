concrete Params2Cnc of Params2 = {

  param Prelude_Bool = Prelude_False | Prelude_True;
  param ResGer_AForm = ResGer_APred | ResGer_AMod ResGer_GenNum;
  param ResGer_GenNum = ResGer_GSg ResGer_Gender | ResGer_GPl;
  param ResGer_Gender = ResGer_Masc | ResGer_Fem | ResGer_Neutr;

  lincat
       Quality = {s : ResGer_AForm => Str};
       MassKind = {s : Str; g : ResGer_Gender};

  lin
    SuchMassKind Quality_0 MassKind_1 =
        {s =
           (MassKind_1.s) ++
           (Quality_0.s)!(ResGer_AMod (ResGer_GSg (MassKind_1.g)));
         g = MassKind_1.g};

    Salt =
        {s = "Salz";
         g = ResGer_Neutr};

    Good =
        {s =
            table {ResGer_APred => "gut";
                   ResGer_AMod (ResGer_GSg ResGer_Masc) => "guter";
                   ResGer_AMod (ResGer_GSg ResGer_Fem) => "gute";
                   ResGer_AMod (ResGer_GSg ResGer_Neutr) => "gutes";
                   ResGer_AMod ResGer_GPl => "gute"}};
}
