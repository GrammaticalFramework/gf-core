--# -path=.:../romance:../abstract:../common:../prelude

concrete CompatibilityIta of Compatibility = CatIta ** open Prelude, CommonRomance in {

-- from Noun 19/4/2008

lin
    NumInt n = {s = \\_ => n.s ; isNum = True ; n = Pl} ;
    OrdInt n = {s = \\_ => n.s ++ "."} ; ---

}