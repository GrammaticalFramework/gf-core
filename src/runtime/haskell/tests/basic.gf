abstract basic = {

cat N; S ;

data z : N ;
     s : N -> N ;

data c : N -> S ;

cat P N ;
fun ind : P z -> ((x:N) -> P x -> P (s x)) -> ((x : N) -> P x) ;

}
