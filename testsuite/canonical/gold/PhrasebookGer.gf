concrete PhrasebookGer of Phrasebook = {
param Prelude_Bool = Prelude_False | Prelude_True;
param ResGer_Agr = ResGer_Ag ResGer_Gender ParamX_Number ParamX_Person;
param ParamX_Number = ParamX_Sg | ParamX_Pl;
param ParamX_Person = ParamX_P1 | ParamX_P2 | ParamX_P3;
param ResGer_Gender = ResGer_Masc | ResGer_Fem | ResGer_Neutr;
param ResGer_Control = ResGer_SubjC | ResGer_ObjC | ResGer_NoC;
param ResGer_PCase = ResGer_NPC ResGer_Case | ResGer_NPP ResGer_CPrep;
param ResGer_CPrep =
    ResGer_CAnDat | ResGer_CInAcc | ResGer_CInDat | ResGer_CZuDat |
    ResGer_CVonDat;
param ResGer_Case = ResGer_Nom | ResGer_Acc | ResGer_Dat | ResGer_Gen;
param ResGer_VAux = ResGer_VHaben | ResGer_VSein;
param ResGer_VForm =
    ResGer_VInf Prelude_Bool | ResGer_VFin Prelude_Bool ResGer_VFormFin |
    ResGer_VImper ParamX_Number | ResGer_VPresPart ResGer_AForm |
    ResGer_VPastPart ResGer_AForm;
param ResGer_AForm = ResGer_APred | ResGer_AMod ResGer_GenNum ResGer_Case;
param ResGer_GenNum = ResGer_GSg ResGer_Gender | ResGer_GPl;
param ResGer_VFormFin =
    ResGer_VPresInd ParamX_Number ParamX_Person |
    ResGer_VPresSubj ParamX_Number ParamX_Person;
param ResGer_VType = ResGer_VAct | ResGer_VRefl ResGer_Case;
lincat PlaceKind = {s : Str};
       VerbPhrase =
           {s :
              {s : ResGer_VForm => Str; aux : ResGer_VAux; particle : Str;
               prefix : Str; vtype : ResGer_VType};
            a1 : Str; a2 : Str; adj : Str; ext : Str;
            inf : {s : Str; ctrl : ResGer_Control; isAux : Prelude_Bool};
            infExt : Str; isAux : Prelude_Bool;
            nn :
              ResGer_Agr =>
              {p1 : Str; p2 : Str; p3 : Str; p4 : Str; p5 : Str; p6 : Str};
            subjc :
              {s : Str; c : ResGer_PCase; isPrep : Prelude_Bool; s2 : Str}};
lin VRead =
        {s =
           {s =
              table {ResGer_VInf Prelude_False => "lesen";
                     ResGer_VInf Prelude_True => "zu" ++ "lesen";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresInd ParamX_Sg ParamX_P1) =>
                       "lese";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresInd ParamX_Sg ParamX_P2) =>
                       "liest";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresInd ParamX_Sg ParamX_P3) =>
                       "liest";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresInd ParamX_Pl ParamX_P1) =>
                       "lesen";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresInd ParamX_Pl ParamX_P2) =>
                       "lest";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresInd ParamX_Pl ParamX_P3) =>
                       "lesen";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresSubj ParamX_Sg ParamX_P1) =>
                       "lese";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresSubj ParamX_Sg ParamX_P2) =>
                       "lesest";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresSubj ParamX_Sg ParamX_P3) =>
                       "lese";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresSubj ParamX_Pl ParamX_P1) =>
                       "lesen";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresSubj ParamX_Pl ParamX_P2) =>
                       "leset";
                     ResGer_VFin Prelude_False
                                 (ResGer_VPresSubj ParamX_Pl ParamX_P3) =>
                       "lesen";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresInd ParamX_Sg ParamX_P1) =>
                       "lese";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresInd ParamX_Sg ParamX_P2) =>
                       "liest";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresInd ParamX_Sg ParamX_P3) =>
                       "liest";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresInd ParamX_Pl ParamX_P1) =>
                       "lesen";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresInd ParamX_Pl ParamX_P2) =>
                       "lest";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresInd ParamX_Pl ParamX_P3) =>
                       "lesen";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresSubj ParamX_Sg ParamX_P1) =>
                       "lese";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresSubj ParamX_Sg ParamX_P2) =>
                       "lesest";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresSubj ParamX_Sg ParamX_P3) =>
                       "lese";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresSubj ParamX_Pl ParamX_P1) =>
                       "lesen";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresSubj ParamX_Pl ParamX_P2) =>
                       "leset";
                     ResGer_VFin Prelude_True
                                 (ResGer_VPresSubj ParamX_Pl ParamX_P3) =>
                       "lesen";
                     ResGer_VImper ParamX_Sg => "les";
                     ResGer_VImper ParamX_Pl => "lest";
                     ResGer_VPresPart ResGer_APred => "lesend";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Nom) =>
                       "lesender";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Acc) =>
                       "lesenden";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Dat) =>
                       "lesendem";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Gen) =>
                       "lesenden";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Nom) =>
                       "lesende";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Acc) =>
                       "lesende";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Dat) =>
                       "lesender";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Gen) =>
                       "lesender";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Nom) =>
                       "lesendes";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Acc) =>
                       "lesendes";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Dat) =>
                       "lesendem";
                     ResGer_VPresPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Gen) =>
                       "lesenden";
                     ResGer_VPresPart (ResGer_AMod ResGer_GPl ResGer_Nom) =>
                       "lesende";
                     ResGer_VPresPart (ResGer_AMod ResGer_GPl ResGer_Acc) =>
                       "lesende";
                     ResGer_VPresPart (ResGer_AMod ResGer_GPl ResGer_Dat) =>
                       "lesenden";
                     ResGer_VPresPart (ResGer_AMod ResGer_GPl ResGer_Gen) =>
                       "lesender";
                     ResGer_VPastPart ResGer_APred => "gelesen";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Nom) =>
                       "gelesener";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Acc) =>
                       "gelesenen";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Dat) =>
                       "gelesenem";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Masc)
                                                   ResGer_Gen) =>
                       "gelesenen";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Nom) =>
                       "gelesene";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Acc) =>
                       "gelesene";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Dat) =>
                       "gelesener";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Fem)
                                                   ResGer_Gen) =>
                       "gelesener";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Nom) =>
                       "gelesenes";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Acc) =>
                       "gelesenes";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Dat) =>
                       "gelesenem";
                     ResGer_VPastPart (ResGer_AMod (ResGer_GSg ResGer_Neutr)
                                                   ResGer_Gen) =>
                       "gelesenen";
                     ResGer_VPastPart (ResGer_AMod ResGer_GPl ResGer_Nom) =>
                       "gelesene";
                     ResGer_VPastPart (ResGer_AMod ResGer_GPl ResGer_Acc) =>
                       "gelesene";
                     ResGer_VPastPart (ResGer_AMod ResGer_GPl ResGer_Dat) =>
                       "gelesenen";
                     ResGer_VPastPart (ResGer_AMod ResGer_GPl ResGer_Gen) =>
                       "gelesener"};
            aux = ResGer_VHaben; particle = ""; prefix = "";
            vtype = ResGer_VAct};
         a1 = ""; a2 = ""; adj = "";
         ext = ""; inf = {s = ""; ctrl = ResGer_NoC; isAux = Prelude_True};
         infExt = ""; isAux = Prelude_False;
         nn =
           table {ResGer_Ag ResGer_Masc ParamX_Sg ParamX_P1 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Masc ParamX_Sg ParamX_P2 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Masc ParamX_Sg ParamX_P3 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Masc ParamX_Pl ParamX_P1 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Masc ParamX_Pl ParamX_P2 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Masc ParamX_Pl ParamX_P3 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Fem ParamX_Sg ParamX_P1 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Fem ParamX_Sg ParamX_P2 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Fem ParamX_Sg ParamX_P3 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Fem ParamX_Pl ParamX_P1 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Fem ParamX_Pl ParamX_P2 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Fem ParamX_Pl ParamX_P3 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Neutr ParamX_Sg ParamX_P1 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Neutr ParamX_Sg ParamX_P2 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Neutr ParamX_Sg ParamX_P3 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Neutr ParamX_Pl ParamX_P1 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Neutr ParamX_Pl ParamX_P2 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""};
                  ResGer_Ag ResGer_Neutr ParamX_Pl ParamX_P3 =>
                    {p1 = ""; p2 = ""; p3 = ""; p4 = ""; p5 = ""; p6 = ""}};
         subjc =
           {s = ""; c = ResGer_NPC ResGer_Nom; isPrep = Prelude_False;
            s2 = ""}};
}