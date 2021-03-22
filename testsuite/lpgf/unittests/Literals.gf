abstract Literals = {
  cat S ;
  fun
    mkString : String -> S ;
    mkInt : Int -> S ;
    mkFloat : Float -> S ;

    and : S -> S -> S ;
}
