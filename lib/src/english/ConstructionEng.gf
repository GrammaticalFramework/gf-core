--# -path=.:../abstract

concrete ConstructionEng of Construction = CatEng ** 
  open SyntaxEng, SymbolicEng, ParadigmsEng, (L = LexiconEng), (E = ExtraEng), (G = GrammarEng), (R = ResEng), (S = StructuralEng), Prelude in {


lin
  hungry_VP = mkVP (mkA "hungry") ;
  thirsty_VP = mkVP (mkA "thirsty") ;
  has_age_VP card = mkVP (mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> L.year_N))) L.old_A) ;

  have_name_Cl x y = mkCl (mkNP (E.GenNP x) L.name_N) (lin NP y) ;
  married_Cl x y = mkCl (lin NP x) L.married_A2 (lin NP y) | mkCl (mkNP and_Conj (lin NP x) (lin NP y)) (mkA "married") ;

  what_name_QCl x = mkQCl (mkIComp whatSg_IP) (mkNP (E.GenNP x) L.name_N) ;
  how_old_QCl x = mkQCl (E.ICompAP (mkAP L.old_A)) (lin NP x) ;
  how_far_QCl x = mkQCl (E.IAdvAdv (ss "far")) (lin NP x) ;

-- some more things
  weather_adjCl ap = mkCl (mkVP (lin AP ap)) ;
   
  is_right_VP = mkVP (ParadigmsEng.mkA "right") ;
  is_wrong_VP = mkVP (ParadigmsEng.mkA "wrong") ;

  n_units_AP card cn a = mkAP (lin AdA (mkUtt (mkNP <lin Card card : Card> (lin CN cn)))) (lin A a) ;


  bottle_of_CN np = mkCN (lin N2 (mkN2 "bottle")) (lin NP np) ;
  cup_of_CN    np = mkCN (lin N2 (mkN2 "cup"))    (lin NP np) ;
  glass_of_CN  np = mkCN (lin N2 (mkN2 "glass"))  (lin NP np) ;

{-
-- spatial deixis and motion verbs

  where_go_QCl np = mkQCl where_IAdv (mkCl np (mkVP L.go_V)) ;
  where_come_from_QCl np =  mkQCl from_where_IAdv (mkCl np (mkVP L.go_V)) ;
  
  go_here_VP = mkVP (mkVP L.go_V) S.here_Adv ;
  come_here_VP = mkVP (mkVP L.come_V) S.here_Adv ;
  come_from_here_VP = mkVP (mkVP L.come_V) (mkAdv "from here") ;

  go_there_VP = mkVP (mkVP L.go_V)  S.there_Adv ;
  come_there_VP = mkVP (mkVP L.come_V) S.there_Adv ;
  come_from_there_VP = mkVP (mkVP L.come_V) (mkAdv "from there") ;

--TODO "where did X come from" instead of "from where did X come"
oper from_where_IAdv : IAdv = lin IAdv (ss "from where") ;

-}


lincat
  Weekday = N ;
  Monthday = NP ;
  Month = N ;
  Year = NP ;
lin
  weekdayPunctualAdv w = SyntaxEng.mkAdv on_Prep (mkNP w) ;         -- on Sunday
  weekdayHabitualAdv w = SyntaxEng.mkAdv on_Prep (mkNP aPl_Det w) ; -- on Sundays
  weekdayNextAdv w = SyntaxEng.mkAdv (mkPrep "next") (mkNP w) ;     -- next Sunday
  weekdayLastAdv w = SyntaxEng.mkAdv (mkPrep "last") (mkNP w) ;     -- last Sunday

  monthAdv m = SyntaxEng.mkAdv in_Prep (mkNP m) ;
  yearAdv y = SyntaxEng.mkAdv in_Prep y ;
  dayMonthAdv d m = ParadigmsEng.mkAdv ("on" ++ d.s ! R.NPAcc ++ m.s ! R.Sg ! R.Nom) ; -- on 17 May
  monthYearAdv m y = SyntaxEng.mkAdv in_Prep (mkNP (mkCN m y)) ; -- in May 2012
  dayMonthYearAdv d m y = ParadigmsEng.mkAdv ("on" ++ d.s ! R.NPAcc ++ m.s ! R.Sg ! R.Nom ++ y.s ! R.NPAcc) ; -- on 17 May 2013

  intYear = symb ;
  intMonthday = symb ;

lincat Language = PN ;

lin InLanguage l = SyntaxEng.mkAdv in_Prep (mkNP l) ;

lin
  weekdayN w = w ;
  monthN m = m ;

  weekdayPN w = mkPN w ;
  monthPN m = mkPN m ;

  languagePN l = l ;

oper mkLanguage : Str -> PN = \s -> mkPN s ;

----------------------------------------------
---- lexicon of special names

lin monday_Weekday = mkN "Monday" ;
lin tuesday_Weekday = mkN "Tuesday" ;
lin wednesday_Weekday = mkN "Wednesday" ;
lin thursday_Weekday = mkN "Thursday" ;
lin friday_Weekday = mkN "Friday" ;
lin saturday_Weekday = mkN "Saturday" ;
lin sunday_Weekday = mkN "Sunday" ;

lin january_Month = mkN "January" ; 
lin february_Month = mkN "February" ; 
lin march_Month = mkN "March" ; 
lin april_Month = mkN "April" ;
lin may_Month = mkN "May" ;
lin june_Month = mkN "June" ;
lin july_Month = mkN "July" ;
lin august_Month = mkN "August" ;
lin september_Month = mkN "September" ;
lin october_Month = mkN "October" ;
lin november_Month = mkN "November" ;
lin december_Month = mkN "December" ;

lin afrikaans_Language = mkLanguage "Afrikaans" ;
lin amharic_Language = mkLanguage "Amharic" ;
lin arabic_Language = mkLanguage "Arabic" ;
lin bulgarian_Language = mkLanguage "Bulgarian" ;
lin catalan_Language = mkLanguage "Catalan" ;
lin chinese_Language = mkLanguage "Chinese" ;
lin danish_Language = mkLanguage "Danish" ;
lin dutch_Language = mkLanguage "Dutch" ;
lin english_Language = mkLanguage "English" ;
lin estonian_Language = mkLanguage "Estonian" ;
lin finnish_Language = mkLanguage "Finnish" ;
lin french_Language = mkLanguage "French" ;
lin german_Language = mkLanguage "German" ;
lin greek_Language = mkLanguage "Greek" ;
lin hebrew_Language = mkLanguage "Hebrew" ;
lin hindi_Language = mkLanguage "Hindi" ;
lin japanese_Language = mkLanguage "Japanese" ;
lin italian_Language = mkLanguage "Italian" ;
lin latin_Language = mkLanguage "Latin" ;
lin latvian_Language = mkLanguage "Latvian" ;
lin maltese_Language = mkLanguage "Maltese" ;
lin nepali_Language = mkLanguage "Nepali" ;
lin norwegian_Language = mkLanguage "Norwegian" ;
lin persian_Language = mkLanguage "Persian" ;
lin polish_Language = mkLanguage "Polish" ;
lin punjabi_Language = mkLanguage "Punjabi" ;
lin romanian_Language = mkLanguage "Romanian" ;
lin russian_Language = mkLanguage "Russian" ;
lin sindhi_Language = mkLanguage "Sindhi" ;
lin spanish_Language = mkLanguage "Spanish" ;
lin swahili_Language = mkLanguage "Swahili" ;
lin swedish_Language = mkLanguage "Swedish" ;
lin thai_Language = mkLanguage "Thai" ;
lin turkish_Language = mkLanguage "Turkish" ;
lin urdu_Language = mkLanguage "Urdu" ;


}