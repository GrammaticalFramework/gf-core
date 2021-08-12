abstract basic = {

cat N; S ;

fun z : N ;
    s : N -> N ;

fun c : N -> S ;

cat P N ;
fun ind : P z -> ((x:N) -> P x -> P (s x)) -> ((x : N) -> P x) ;

}
