abstract basic = {

cat N; S ;

data z : N ;
     s : N -> N ;

data c : N -> S ;

cat P N ;
fun nat : (x : N) -> P x ;
fun ind : P z -> ((x:N) -> P x -> P (s x)) -> ((x : N) -> P x) ;

fun intLit    : Int -> S;
fun stringLit : String -> S;
fun floatLit  : Float -> S;

}
