concrete ProjectionCnc of Projection = {
  -- param Case = Nom | Acc ;

  lincat
    Comment = {s : Str} ;
    Item = {
      -- s : Case => {comp : Str} ;
      nom : {comp: Str}
    } ;
  lin
    Wine = {
      -- s = table {
      --   Nom => {comp = "ce" ++ "vin"} ;
      --   Acc => {comp = "ce" ++ "vin"}
      -- } ;
      nom = {comp = "ce" ++ "vin"} ;
    } ;
    -- Pred item = { s = ((item.s)!Nom).comp } ;
    Pred item = { s = (item.nom).comp } ;
}
