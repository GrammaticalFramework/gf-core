concrete PreCnc of Pre = {
  lincat
    S = { s : Str } ;
    N = { s : Str } ;
    Det = { s : Str } ;
  lin
    ant = { s = "ant" } ;
    dog = { s = "dog" } ;
    a = { s = pre {
      "a"|"e"|"i"|"o"|"u" => "an" ;
      _ => "a"
    } } ;
    the = { s = "the" } ;
    mkS det n = { s = det.s ++ n.s } ;
    detS det = { s = det.s } ;
}
