abstract Bind = {
  cat S ;
  fun
    f1 : S ;
    f2 : S ;
    concat : S -> S -> S ;
    bind : S -> S -> S ;
    softbind : S -> S -> S ;
    softspace : S -> S -> S ;
    capit : S -> S ;
    allcapit : S -> S ;
}
