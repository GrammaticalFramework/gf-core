--# -path=.:present

-- (c) 2009 Aarne Ranta under LGPL

concrete FoodsLat of Foods = LexFoodsLat **
{
  lincat
    Comment = { s : Str } ;
    Item = { number : Number ; gender : Gender; noun : Str; adj : Str; det : Str };
  lin
    Mod quality kind =
      variants {
	{
	  gender = kind.gender ;
	  noun = table { number => kind.noun ! number ++ quality.s ! number ! kind.gender } ;
	  adj = kind.adj
	  } ;
	{
	  gender = kind.gender ;
	  noun = kind.noun ;
	  adj = table { number => kind.adj ! number ++ quality.s ! number ! kind.gender }
	  } ;
	{
	  gender = kind.gender ;
	  noun = table { number => quality.s ! number ! kind.gender ++ kind.noun ! number } ;
	  adj = kind.adj
	  } ;
       	{
      	  gender = kind.gender ;
      	  noun = kind.noun ;
      	  adj = table { number => quality.s ! number ! kind.gender ++ kind.adj ! number }
      	  }
      };
    Pred item quality =
      let aux : Number => Str =
	    table { Sg => "est" ; Pl => "sunt" } ;
      in
      {
	s = variants {
	  item.det ++ item.noun ++ item.adj ++ aux ! item.number ++ quality.s ! item.number ! item.gender ;
	  item.det ++ item.adj ++ item.noun ++ aux ! item.number ++ quality.s ! item.number ! item.gender ;
	  item.det ++ item.noun ++ item.adj ++ quality.s ! item.number ! item.gender ++ aux ! item.number ;
	  item.det ++ item.adj ++ item.noun  ++ quality.s ! item.number ! item.gender ++ aux ! item.number
	  };
      };
    This kind = {
      number = Sg ;
      gender = kind.gender ;
      noun = kind.noun ! Sg ;
      adj = kind.adj ! Sg ;
      det = table { Male => "hic" ; Female => "haec" ; Neuter => "hoc" } ! kind.gender
      } ;
    These kind = {
      number = Pl ;
      gender = kind.gender ;
      noun = kind.noun ! Pl ;
      adj = kind.adj ! Pl ;
      det = table { Male => "hi" ; Female => "hae" ; Neuter => "haec" } ! kind.gender
      } ;
    That kind = {
      number = Sg ;
      gender = kind.gender ;
      noun = kind.noun ! Sg ;
      adj = kind.adj ! Sg ;
      det = table { Male => "is" ; Female => "ea" ; Neuter => "id" } ! kind.gender
      } ;
    Those kind = {
      number = Pl ;
      gender = kind.gender ;
      noun = kind.noun ! Pl ;
      adj = kind.adj ! Pl ;
      det = table { Male => variants { "ei "; "ii" } ; Female => "eae" ; Neuter => "ea" } ! kind.gender
      } ;
    Very quality = { s = \\n,g => "valde" ++ quality.s ! n ! g };
}

