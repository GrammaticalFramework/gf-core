concrete FoodsFin of Foods = {
param ParamX_Number = ParamX_Sg | ParamX_Pl;
param Prelude_Bool = Prelude_False | Prelude_True;
param ResFin_Agr = ResFin_Ag ParamX_Number ParamX_Person | ResFin_AgPol;
param ParamX_Person = ParamX_P1 | ParamX_P2 | ParamX_P3;
param ResFin_Harmony = ResFin_Back | ResFin_Front;
param ResFin_NForm =
    ResFin_NCase ParamX_Number ResFin_Case | ResFin_NComit | ResFin_NInstruct |
    ResFin_NPossNom ParamX_Number | ResFin_NPossGen ParamX_Number |
    ResFin_NPossTransl ParamX_Number | ResFin_NPossIllat ParamX_Number |
    ResFin_NCompound;
param ResFin_Case =
    ResFin_Nom | ResFin_Gen | ResFin_Part | ResFin_Transl | ResFin_Ess |
    ResFin_Iness | ResFin_Elat | ResFin_Illat | ResFin_Adess | ResFin_Ablat |
    ResFin_Allat | ResFin_Abess;
param ResFin_NPForm = ResFin_NPCase ResFin_Case | ResFin_NPAcc | ResFin_NPSep;
lincat Comment = {s : Str};
       Item =
           {s : ResFin_NPForm => Str; a : ResFin_Agr; isNeg : Prelude_Bool;
            isPron : Prelude_Bool};
       Kind =
           {s : ResFin_NForm => Str; h : ResFin_Harmony;
            postmod : ParamX_Number => Str};
       Quality =
           {s : Prelude_Bool => ResFin_NForm => Str; hasPrefix : Prelude_Bool;
            p : Str};
lin Expensive =
        {s =
           table {Prelude_False =>
                    table {ResFin_NCase ParamX_Sg ResFin_Nom => "kallis";
                           ResFin_NCase ParamX_Sg ResFin_Gen => "kalliin";
                           ResFin_NCase ParamX_Sg ResFin_Part => "kallista";
                           ResFin_NCase ParamX_Sg ResFin_Transl => "kalliiksi";
                           ResFin_NCase ParamX_Sg ResFin_Ess => "kalliina";
                           ResFin_NCase ParamX_Sg ResFin_Iness => "kalliissa";
                           ResFin_NCase ParamX_Sg ResFin_Elat => "kalliista";
                           ResFin_NCase ParamX_Sg ResFin_Illat => "kalliiseen";
                           ResFin_NCase ParamX_Sg ResFin_Adess => "kalliilla";
                           ResFin_NCase ParamX_Sg ResFin_Ablat => "kalliilta";
                           ResFin_NCase ParamX_Sg ResFin_Allat => "kalliille";
                           ResFin_NCase ParamX_Sg ResFin_Abess => "kalliitta";
                           ResFin_NCase ParamX_Pl ResFin_Nom => "kalliit";
                           ResFin_NCase ParamX_Pl ResFin_Gen => "kalliiden";
                           ResFin_NCase ParamX_Pl ResFin_Part => "kalliita";
                           ResFin_NCase ParamX_Pl ResFin_Transl => "kalliiksi";
                           ResFin_NCase ParamX_Pl ResFin_Ess => "kalliina";
                           ResFin_NCase ParamX_Pl ResFin_Iness => "kalliissa";
                           ResFin_NCase ParamX_Pl ResFin_Elat => "kalliista";
                           ResFin_NCase ParamX_Pl ResFin_Illat => "kalliisiin";
                           ResFin_NCase ParamX_Pl ResFin_Adess => "kalliilla";
                           ResFin_NCase ParamX_Pl ResFin_Ablat => "kalliilta";
                           ResFin_NCase ParamX_Pl ResFin_Allat => "kalliille";
                           ResFin_NCase ParamX_Pl ResFin_Abess => "kalliitta";
                           ResFin_NComit => "kalliine";
                           ResFin_NInstruct => "kalliin";
                           ResFin_NPossNom ParamX_Sg => "kallii";
                           ResFin_NPossNom ParamX_Pl => "kallii";
                           ResFin_NPossGen ParamX_Sg => "kallii";
                           ResFin_NPossGen ParamX_Pl => "kalliide";
                           ResFin_NPossTransl ParamX_Sg => "kalliikse";
                           ResFin_NPossTransl ParamX_Pl => "kalliikse";
                           ResFin_NPossIllat ParamX_Sg => "kalliisee";
                           ResFin_NPossIllat ParamX_Pl => "kalliisii";
                           ResFin_NCompound => "kallis"};
                  Prelude_True =>
                    table {ResFin_NCase ParamX_Sg ResFin_Nom => "kallis";
                           ResFin_NCase ParamX_Sg ResFin_Gen => "kalliin";
                           ResFin_NCase ParamX_Sg ResFin_Part => "kallista";
                           ResFin_NCase ParamX_Sg ResFin_Transl => "kalliiksi";
                           ResFin_NCase ParamX_Sg ResFin_Ess => "kalliina";
                           ResFin_NCase ParamX_Sg ResFin_Iness => "kalliissa";
                           ResFin_NCase ParamX_Sg ResFin_Elat => "kalliista";
                           ResFin_NCase ParamX_Sg ResFin_Illat => "kalliiseen";
                           ResFin_NCase ParamX_Sg ResFin_Adess => "kalliilla";
                           ResFin_NCase ParamX_Sg ResFin_Ablat => "kalliilta";
                           ResFin_NCase ParamX_Sg ResFin_Allat => "kalliille";
                           ResFin_NCase ParamX_Sg ResFin_Abess => "kalliitta";
                           ResFin_NCase ParamX_Pl ResFin_Nom => "kalliit";
                           ResFin_NCase ParamX_Pl ResFin_Gen => "kalliiden";
                           ResFin_NCase ParamX_Pl ResFin_Part => "kalliita";
                           ResFin_NCase ParamX_Pl ResFin_Transl => "kalliiksi";
                           ResFin_NCase ParamX_Pl ResFin_Ess => "kalliina";
                           ResFin_NCase ParamX_Pl ResFin_Iness => "kalliissa";
                           ResFin_NCase ParamX_Pl ResFin_Elat => "kalliista";
                           ResFin_NCase ParamX_Pl ResFin_Illat => "kalliisiin";
                           ResFin_NCase ParamX_Pl ResFin_Adess => "kalliilla";
                           ResFin_NCase ParamX_Pl ResFin_Ablat => "kalliilta";
                           ResFin_NCase ParamX_Pl ResFin_Allat => "kalliille";
                           ResFin_NCase ParamX_Pl ResFin_Abess => "kalliitta";
                           ResFin_NComit => "kalliine";
                           ResFin_NInstruct => "kalliin";
                           ResFin_NPossNom ParamX_Sg => "kallii";
                           ResFin_NPossNom ParamX_Pl => "kallii";
                           ResFin_NPossGen ParamX_Sg => "kallii";
                           ResFin_NPossGen ParamX_Pl => "kalliide";
                           ResFin_NPossTransl ParamX_Sg => "kalliikse";
                           ResFin_NPossTransl ParamX_Pl => "kalliikse";
                           ResFin_NPossIllat ParamX_Sg => "kalliisee";
                           ResFin_NPossIllat ParamX_Pl => "kalliisii";
                           ResFin_NCompound => "kallis"}};
         hasPrefix = Prelude_False; p = ""};
}
