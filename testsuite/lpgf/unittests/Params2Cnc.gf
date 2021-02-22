concrete Params2Cnc of Params2 = {

  param ParamX_Number = ParamX_Sg | ParamX_Pl;
  param Prelude_Bool = Prelude_False | Prelude_True;
  param ResGer_Adjf = ResGer_Strong | ResGer_Weak;
  param ResGer_Case =
      ResGer_Nom | ResGer_Acc | ResGer_Dat | ResGer_Gen;
  param ResGer_AForm =
      ResGer_APred | ResGer_AMod ResGer_GenNum ResGer_Case;
  param ResGer_GenNum = ResGer_GSg ResGer_Gender | ResGer_GPl;
  param ResGer_Gender = ResGer_Masc | ResGer_Fem | ResGer_Neutr;

  lincat
       Quality = {s : ResGer_AForm => Str};
       MassKind =
           {s : ResGer_Adjf => ParamX_Number => ResGer_Case => Str;
            g : ResGer_Gender; };

  lin
    SuchMassKind Quality_0 MassKind_1 =
        {s =
          table {_ => -- Strong/Weak
            table {_ => -- Sg/Pl
              table {_ => -- Nom/Acc/Dat/Gen
                 (((MassKind_1.s)!ResGer_Strong)!ParamX_Sg)!ResGer_Nom ++
                 (Quality_0.s)!(ResGer_AMod (ResGer_GSg (MassKind_1.g)) ResGer_Nom)}}};
         g = MassKind_1.g};

    Salt =
        {s =
           table {ResGer_Strong =>
                    table {ParamX_Sg =>
                             table {ResGer_Nom => "Salz"; ResGer_Acc => "Salz";
                                    ResGer_Dat => "Salz"; ResGer_Gen => "Salzes"};
                           ParamX_Pl =>
                             table {ResGer_Nom => "Salze"; ResGer_Acc => "Salze";
                                    ResGer_Dat => "Salzen"; ResGer_Gen => "Salze"}};
                  ResGer_Weak =>
                    table {ParamX_Sg =>
                             table {ResGer_Nom => "Salz"; ResGer_Acc => "Salz";
                                    ResGer_Dat => "Salz"; ResGer_Gen => "Salzes"};
                           ParamX_Pl =>
                             table {ResGer_Nom => "Salze"; ResGer_Acc => "Salze";
                                    ResGer_Dat => "Salzen"; ResGer_Gen => "Salze"}}};
         g = ResGer_Neutr};

    Good =
        {s =
            table {ResGer_APred => "gut";
                   ResGer_AMod (ResGer_GSg ResGer_Masc) ResGer_Nom => "guter";
                   ResGer_AMod (ResGer_GSg ResGer_Masc) ResGer_Acc => "guten";
                   ResGer_AMod (ResGer_GSg ResGer_Masc) ResGer_Dat => "gutem";
                   ResGer_AMod (ResGer_GSg ResGer_Masc) ResGer_Gen => "guten";
                   ResGer_AMod (ResGer_GSg ResGer_Fem) ResGer_Nom => "gute";
                   ResGer_AMod (ResGer_GSg ResGer_Fem) ResGer_Acc => "gute";
                   ResGer_AMod (ResGer_GSg ResGer_Fem) ResGer_Dat => "guter";
                   ResGer_AMod (ResGer_GSg ResGer_Fem) ResGer_Gen => "guter";
                   ResGer_AMod (ResGer_GSg ResGer_Neutr) ResGer_Nom => "gutes";
                   ResGer_AMod (ResGer_GSg ResGer_Neutr) ResGer_Acc => "gutes";
                   ResGer_AMod (ResGer_GSg ResGer_Neutr) ResGer_Dat => "gutem";
                   ResGer_AMod (ResGer_GSg ResGer_Neutr) ResGer_Gen => "guten";
                   ResGer_AMod ResGer_GPl ResGer_Nom => "gute";
                   ResGer_AMod ResGer_GPl ResGer_Acc => "gute";
                   ResGer_AMod ResGer_GPl ResGer_Dat => "guten";
                   ResGer_AMod ResGer_GPl ResGer_Gen => "guter"}};
}
