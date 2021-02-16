-- (c) 2009 Aarne Ranta under LGPL

incomplete concrete LexFoodsLat of Foods = 
  {
  param
    Gender = Male|Female|Neuter;
    Number = Sg|Pl;
  oper
    regA : Str -> { s : Number => Gender => Str } =
      \a -> {
	s = case a of {
	  stem + "us" => table { Sg => table { Male => a ;
					       Female => stem + "a" ;
					       Neuter => stem + "um"
				   };
				 Pl => table { Male => stem + "i" ;
					       Female => stem + "ae" ;
					       Neuter => stem + "a"
				   }
	    } ;
	  _ => \\_,_ => ""
	  }
      } ;
    regN : Str -> { gender : Gender ; noun : Number => Str; adj : Number => Str } =
      \nomsg ->
      let nounpart : { gender : Gender ; noun : Number => Str } = 
	    case nomsg of {
	      stem + "us" => { gender = Male; noun = table { Sg => nomsg ; Pl => stem + "i" } ; } ; 
	      stem + "a" => { gender = Female; noun = table { Sg => nomsg ; Pl => stem + "ae" } } ; 
	      stem + "um" => { gender = Neuter; noun = table { Sg => nomsg ; Pl => stem + "i" } } ;
	      _ => { gender = Neuter; noun = \\_ => "" }
	    };
      in
      nounpart ** { adj = \\_ => "" } ;
    irregN : Str -> Str -> Gender -> { gender : Gender ; noun : Number => Str; adj : Number => Str } =
      \nomsg,nompl,gender ->
      { gender = gender ; noun = table { Sg => nomsg ; Pl => nompl } ; adj = \\_ => "" } ;
      
  lincat
    Kind = { gender : Gender; noun : Number => Str; adj: Number => Str };
    Quality = { s : Number => Gender => Str } ;
  lin
    Wine = regN "vinum" ;
    Pizza = { gender = Female ; noun = table { Sg => "placenta" ; Pl => "placentae" } ; adj = table { Sg => "neapolitana" ; Pl => "neapolitanae" } } ;
    Cheese = regN "formaticum" ;
    Fish = irregN "piscis" "pisces" Male ;
    Fresh = { s = table { Sg => \\_ => "recens" ; Pl => table { Male|Female => "recentes" ; Neuter => "recentia" } } } ;
    Warm = regA "calidus" ;
    Italian = regA "italus" ;
    Expensive = regA "pretiosus" ;
    Delicious = regA "iucundus" ;
    Boring = { s = table { Sg => \\_ => "fluens" ; Pl => table { Male|Female => "fluentes" ; Neuter => "recentia" } } };
}
