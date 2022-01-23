concrete FoodsChi of Foods = open Prelude in {
flags coding = utf8 ;
lincat
    Comment, Item = Str;
    Kind = knd ;
    Quality = qual ;
lin
    Pred = (\itm, ql ->
      case ql.hasVery of {
        True  => itm ++ "是 非 常" ++ ql.s ++ ql.p ;
        False => itm ++ "是"       ++ ql.s ++ ql.p } ) ;
    This kind = "这" ++ kind.c ++ kind.m ++ kind.s ;
    That kind = "那" ++ kind.c ++ kind.m ++ kind.s ;
    These kind = "这" ++ "几" ++ kind.c ++ kind.m ++ kind.s ;
    Those kind = "那" ++ "几" ++ kind.c ++ kind.m ++ kind.s ;
    Mod = modifier ;

    Wine  = geKind "酒" "瓶" ;
    Pizza = geKind "比 萨 饼" "张" ;
    Cheese  = geKind "奶 酪" "块";
    Fish  = geKind "鱼" "条";

    Very = (\q -> {s = q.s ; p = q.p ; hasVery = True}) ;
    Fresh  = longQuality "新 鲜" ;
    Warm  = longQuality "温 热" ;
    Italian  = longQuality "意 大 利 式" ;
    Expensive  = longQuality "昂 贵" ;
    Delicious  = longQuality "美 味" ;
     -- this technically translates to "unpalatable" instead of boring
    Boring  = longQuality "难 吃" ;

oper
    -- lincat aliases
    qual : Type = {s,p : Str ; hasVery : Bool} ;
    knd  : Type = {s,c,m : Str; hasMod : Bool} ;

    -- Constructor functions
    mkKind : Str -> Str -> knd = \s,c ->
      {s = s ; c = c; m = ""; hasMod = False} ;
    geKind : Str -> Str -> knd = \s,cl ->
      mkKind s (classifier cl) ;
    longQuality : Str -> qual = \s ->
      {s = s ; p = "的" ; hasVery = False}  ;
    modifier : qual -> knd -> knd = \q,k ->
      { s = k.s ; c = k.c ; m = modJoin k.hasMod q k.m ;
        hasMod = True } ;

    -- Helper functions
    classifier : Str -> Str = \s ->
      case s of {"" => "个" ; _  => s };
    modJoin : Bool -> qual  -> Str -> Str = \bool, q,m ->
      case bool of {
        True  => "又" ++ q.s ++ "又" ++ m ;
        False => q.s ++ q.p } ;

}
