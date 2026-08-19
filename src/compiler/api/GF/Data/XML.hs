    ----------------------------------------------------------------------
-- |
-- Module      : XML
--
-- Utilities for creating XML documents.
----------------------------------------------------------------------
module GF.Data.XML (XML(..), Attr, comments, showXMLDoc, showsXMLDoc, showsXML, showsNospaceXML, bottomUpXML, parseXML) where

import Data.Char(isSpace)
import Numeric (readHex)
import GF.Data.Utilities

data XML = Data String | Tag String [Attr] [XML] | ETag String [Attr] | Comment String | Empty
 deriving (Ord,Eq,Show)

type Attr = (String,String)

comments :: [String] -> [XML]
comments = map Comment

showXMLDoc :: XML -> String
showXMLDoc xml = showsXMLDoc xml ""

showsXMLDoc :: XML -> ShowS
showsXMLDoc xml = showString header . showsXML xml
  where header = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"

showsXML :: XML -> ShowS
showsXML = showsX 0 where
  showsX i x = ind i . case x of
    (Data s) -> showString (escape s)
    (ETag t as) -> showChar '<' . showString t . showsAttrs as . showString "/>"
    (Tag t as cs) -> 
      showChar '<' . showString t . showsAttrs as . showChar '>' . 
      concatS (map (showsX (i+1)) cs) . ind i . 
      showString "</" . showString t . showChar '>'
    (Comment c) -> showString "<!-- " . showString c . showString " -->"
    (Empty) -> id
  ind i = showString ("\n" ++ replicate (2*i) ' ')

showsNospaceXML :: XML -> ShowS
showsNospaceXML x = case x of
    (Data s) -> showString (escape s)
    (ETag t as) -> showChar '<' . showString t . showsAttrs as . showString "/>"
    (Tag t as cs) ->
      showChar '<' . showString t . showsAttrs as . showChar '>' .
      concatS (map showsNospaceXML cs) .
      showString "</" . showString t . showChar '>'
    (Comment c) -> showString "<!-- " . showString c . showString " -->"
    (Empty) -> id

showsAttrs :: [Attr] -> ShowS
showsAttrs = concatS . map (showChar ' ' .) . map showsAttr

showsAttr :: Attr -> ShowS
showsAttr (n,v) = showString n . showString "=\"" . showString (escape v) . showString "\""

escape :: String -> String
escape = concatMap escChar
  where
  escChar '<'  = "&lt;"
  escChar '>'  = "&gt;"
  escChar '&'  = "&amp;"
  escChar '"'  = "&quot;"
  escChar c    = [c]

bottomUpXML :: (XML -> XML) -> XML -> XML
bottomUpXML f (Tag n attrs cs) = f (Tag n attrs (map (bottomUpXML f) cs))
bottomUpXML f x = f x


-- Lexer -----------------------------------------------------------------------

type Line               = Integer
type LChar              = (Line,Char)
type LString            = [LChar]
data Token              = TokStart Line String [Attr] Bool  -- is empty?
                        | TokEnd Line String
                        | TokCRef String
                        | TokText String
                          deriving Show

tokens             :: String -> [Token]
tokens = tokens' . linenumber 1

tokens' :: LString -> [Token]
tokens' ((_,'<') : c@(_,'!') : cs) = special c cs

tokens' ((_,'<') : cs)   = tag (dropSpace cs) -- we are being nice here
tokens' [] = []
tokens' cs@((l,_):_) = let (as,bs) = breakn ('<' ==) cs
                       in map cvt (decode_text as) ++ tokens' bs

  -- XXX: Note, some of the lines might be a bit inacuarate
  where cvt (TxtBit x)  = TokText x
        cvt (CRefBit x) = case cref_to_char x of
                            Just c -> TokText [c]
                            Nothing -> TokCRef x


special :: LChar -> LString -> [Token]
special _ ((_,'-') : (_,'-') : cs) = skip cs
  where skip ((_,'-') : (_,'-') : (_,'>') : ds) = tokens' ds
        skip (_ : ds) = skip ds
        skip [] = [] -- unterminated comment

special c ((_,'[') : (_,'C') : (_,'D') : (_,'A') : (_,'T') : (_,'A') : (_,'[')
         : cs) =
  let (xs,ts) = cdata cs
  in TokText xs : tokens' ts
  where cdata ((_,']') : (_,']') : (_,'>') : ds) = ([],ds)
        cdata ((_,d) : ds)  = let (xs,ys) = cdata ds in (d:xs,ys)
        cdata []        = ([],[])

special c cs =
  let (xs,ts) = munch "" 0 cs
  in TokText ('<':'!':(reverse xs)) : tokens' ts
  where munch acc nesting ((_,'>') : ds)
         | nesting == (0::Int) = ('>':acc,ds)
         | otherwise           = munch ('>':acc) (nesting-1) ds
        munch acc nesting ((_,'<') : ds)
         = munch ('<':acc) (nesting+1) ds
        munch acc n ((_,x) : ds) = munch (x:acc) n ds
        munch acc _ [] = (acc,[]) -- unterminated DTD markup

--special c cs = tag (c : cs) -- invalid specials are processed as tags

linenumber :: Integer -> String -> LString
linenumber n s = 
  case s of
    [] -> []
    ('\r':s') -> case s' of
                   ('\n':s'') -> next s''
                   _          -> next s'
    ('\n':s') -> next s'
    (c   :s') -> (n,c) : linenumber n s'
  where
    next s' = n' `seq` ((n,'\n'):linenumber n' s') where n' = n + 1


qualName           :: LString -> (String,LString)
qualName xs         = breakn endName xs
  where endName x = isSpace x || x == '=' || x == '>' || x == '/'





tag              :: LString -> [Token]
tag ((p,'/') : cs)    = let (n,ds) = qualName (dropSpace cs)
                        in TokEnd p n : case (dropSpace ds) of
                                          (_,'>') : es -> tokens' es
                                          -- tag was not properly closed...
                                          _        -> tokens' ds
tag []            = []
tag cs            = let (n,ds)  = qualName cs
                        (as,b,ts) = attribs (dropSpace ds)
                    in TokStart (fst (head cs)) n as b : ts

attribs          :: LString -> ([Attr], Bool, [Token])
attribs cs        = case cs of
                      (_,'>') : ds -> ([], False, tokens' ds)

                      (_,'/') : ds -> ([], True, case ds of
                                              (_,'>') : es -> tokens' es
                                              -- insert missing >  ...
                                              _ -> tokens' ds)

                      (_,'?') : (_,'>') : ds -> ([], True, tokens' ds)

                      -- doc ended within a tag..
                      []       -> ([],False,[])

                      _        -> let (a,cs1) = attrib cs
                                      (as,b,ts) = attribs cs1
                                  in (a:as,b,ts)

attrib             :: LString -> (Attr,LString)
attrib cs           = let (ks,cs1)  = qualName cs
                          (vs,cs2)  = attr_val (dropSpace cs1)
                      in ((ks,decode_attr vs),dropSpace cs2)

attr_val           :: LString -> (String,LString)
attr_val ((_,'=') : cs) = string (dropSpace cs)
attr_val cs         = ("",cs)


dropSpace :: LString -> LString
dropSpace = dropWhile (isSpace . snd)

-- | Match the value for an attribute.  For malformed XML we do
-- our best to guess the programmer's intention.
string             :: LString -> (String,LString)
string ((_,'"') : cs)   = break' ('"' ==) cs

-- Allow attributes to be enclosed between ' '.
string ((_,'\'') : cs)  = break' ('\'' ==) cs

-- Allow attributes that are not enclosed by anything.
string cs           = breakn eos cs
  where eos x = isSpace x || x == '>' || x == '/'


break' :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
break' p xs         = let (as,bs) = breakn p xs
                      in (as, case bs of
                                [] -> []
                                _ : cs -> cs)

breakn :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
breakn p l = (map snd as,bs) where (as,bs) = break (p . snd) l



decode_attr :: String -> String
decode_attr cs = concatMap cvt (decode_text cs)
  where cvt (TxtBit x) = x
        cvt (CRefBit x) = case cref_to_char x of
                            Just c -> [c]
                            Nothing -> '&' : x ++ ";"

data Txt = TxtBit String | CRefBit String deriving Show

decode_text :: [Char] -> [Txt]
decode_text xs@('&' : cs) = case break (';' ==) cs of
                              (as,_:bs) -> CRefBit as : decode_text bs
                              _ -> [TxtBit xs]
decode_text []  = []
decode_text cs  = let (as,bs) = break ('&' ==) cs
                  in TxtBit as : decode_text bs

cref_to_char :: [Char] -> Maybe Char
cref_to_char cs = case cs of
  '#' : ds  -> num_esc ds
  "lt"      -> Just '<'
  "gt"      -> Just '>'
  "amp"     -> Just '&'
  "apos"    -> Just '\''
  "quot"    -> Just '"'
  _         -> Nothing

num_esc :: String -> Maybe Char
num_esc cs = case cs of
               'x' : ds -> check (readHex ds)
               _        -> check (reads cs)

  where check [(n,"")]  = cvt_char n
        check _         = Nothing

cvt_char :: Int -> Maybe Char
cvt_char x
  | fromEnum (minBound :: Char) <= x && x <= fromEnum (maxBound::Char)
                = Just (toEnum x)
  | otherwise = Nothing


-- Parser --------------------------------------------------------------

-- | parseXML to a list of content chunks
parseXML :: String -> [XML]
parseXML  = parse . tokens

------------------------------------------------------------------------

parse      :: [Token] -> [XML]
parse []    = []
parse ts    = let (es,_,ts1) = nodes [] ts
              in es ++ parse ts1

nodes :: [String] -> [Token] -> ([XML], [String], [Token])
nodes ps (TokCRef ref : ts) =
  let (es,qs,ts1) = nodes ps ts
  in (Data ref : es, qs, ts1)
nodes ps (TokText txt : ts) =
  let (es,qs,ts1) = nodes ps ts
      (more,es1)  = case es of
                      Data cd : es1' -> (cd,es1')
                      _              -> ([],es)
  in (Data (txt ++ more) : es1, qs, ts1)
nodes ps (TokStart p t as empty : ts) = (node : siblings, open, toks)
  where
    (node,(siblings,open,toks))
      | empty     = (ETag t as, nodes ps ts)
      | otherwise = let (es1,qs1,ts1) = nodes (t:ps) ts
                    in (Tag t as es1,
                        case qs1 of
                          [] -> nodes ps ts1
                          _ : qs3 -> ([],qs3,ts1))
nodes ps (TokEnd p t : ts)   = case break (t ==) ps of
                                  (as,_:_) -> ([],as,ts)
                                  -- Unknown closing tag. Insert as text.
                                  (_,[]) ->
                                    let (es,qs,ts1) = nodes ps ts
                                    in (Data "" : es,qs,ts1)
nodes ps []                 = ([],ps,[])
