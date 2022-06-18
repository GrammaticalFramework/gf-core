concrete PhrasebookBul of Phrasebook = {
param Prelude_Bool = Prelude_False | Prelude_True;
param ResBul_AGender = ResBul_AMasc ResBul_Animacy | ResBul_AFem | ResBul_ANeut;
param ResBul_Animacy = ResBul_Human | ResBul_NonHuman;
param ResBul_Case = ResBul_Acc | ResBul_Dat | ResBul_WithPrep | ResBul_CPrep;
param ResBul_NForm =
    ResBul_NF ParamX_Number ResBul_Species | ResBul_NFSgDefNom |
    ResBul_NFPlCount | ResBul_NFVocative;
param ParamX_Number = ParamX_Sg | ParamX_Pl;
param ResBul_Species = ResBul_Indef | ResBul_Def;
lincat PlaceKind =
           {at : {s : Str; c : ResBul_Case}; isPl : Prelude_Bool;
            name : {s : ResBul_NForm => Str; g : ResBul_AGender};
            to : {s : Str; c : ResBul_Case}};
       VerbPhrase = {s : Str};
lin Airport =
        {at = {s = "на"; c = ResBul_Acc}; isPl = Prelude_False;
         name =
           {s =
              table {ResBul_NF ParamX_Sg ResBul_Indef => "летище";
                     ResBul_NF ParamX_Sg ResBul_Def => "летището";
                     ResBul_NF ParamX_Pl ResBul_Indef => "летища";
                     ResBul_NF ParamX_Pl ResBul_Def => "летищата";
                     ResBul_NFSgDefNom => "летището";
                     ResBul_NFPlCount => "летища";
                     ResBul_NFVocative => "летище"};
            g = ResBul_ANeut};
         to = {s = "до"; c = ResBul_CPrep}};
}