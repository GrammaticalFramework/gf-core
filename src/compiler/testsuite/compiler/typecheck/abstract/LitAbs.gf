abstract LitAbs = {

cat CStr String ;
    CInt Int ;
    CFloat Float ;

data empty : CStr "" ;
    -- null  : CStr [] ; -- Commented out by IL 06/2021: causes parse error
     other : CStr "other" ;

data zero : CInt 0 ;

data pi : CFloat 3.14 ;

}