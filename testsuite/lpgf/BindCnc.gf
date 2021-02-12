concrete BindCnc of Bind = open Prelude in {
  lincat
    S = Str ;
    F = { s : Str } ;
  lin
    f1 = { s = "hello the" ++ BIND ++ "re" } ;
    f2 = { s = "good" ++ "bye" } ;
    FtoS f = f.s ;
}
