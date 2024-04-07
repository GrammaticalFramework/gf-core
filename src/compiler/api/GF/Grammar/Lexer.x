-- -*- haskell -*-
{
{-# LANGUAGE CPP #-}
module GF.Grammar.Lexer
         ( Lang(..), Token(..), Posn(..)
         , P, runP, runLangP, runPartial, token, lexer, getPosn, failLoc
         , isReservedWord
         ) where

import Control.Applicative
import Control.Monad(ap,mplus)
import GF.Infra.Ident
--import GF.Data.Operations
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as WBS
import qualified Data.ByteString.Internal as BS(w2c)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import Data.Word(Word8)
import Data.Char(readLitChar,isSpace,isAlphaNum)
import Data.Maybe(isJust)
import qualified Control.Monad.Fail as Fail
}


$l = [a-zA-Z\192 - \255] # [\215 \247]
$c = [A-Z\192-\221] # [\215]
$s = [a-z\222-\255] # [\247]
$d = [0-9]                -- digit
$i = [$l $d _ ']          -- identifier character
$u = [.\n]                -- universal: any character

@rsyms =    -- symbols and non-identifier-like reserved words
   \; | \= | \{ | \} | \( | \) | \~ | \* \* | \: | \- \> | \, | \[ | \] | \- | \. | \| | \% | \? | \< | \> | \@ | \# | \! | \* | \+ | \+ \+ | \\ | \\\\ | \= \> | \_ | \$ | \/ | \: \= | \: \: \=

@ident =
   (\_ | $l)($l | $d | \_ | \')*

:-
"--" [.]* ; -- Toss single line comments
"{-" ([$u # \-] | \- [$u # \}])* ("-")+ "}" ;

$white+ ;
@rsyms                          { tok ident }
\< $white* @ident $white* (\/ | \>)
                                { \lang inp@(AI pos s) inp' _ -> 
                                     let inp0 = AI (alexMove pos '<') (BS.tail s)
                                     in case lang of {
                                          NLG -> case getTag inp0 of {
                                                   Just (tag,inp') -> POk (inp,inp') (T_open_tag tag) ;
                                                   Nothing         -> PFailed pos "matching the html tag failed"
                                                 } ;
                                          _   -> POk (inp,inp0) T_less
                                        } }
\< $white* @ident $white+ @ident $white* \=
                                { \lang inp@(AI pos s) inp' _ -> 
                                     let inp0 = AI (alexMove pos '<') (BS.tail s)
                                     in case lang of {
                                          NLG -> case getTag inp0 of {
                                                   Just (tag,inp') -> if tag == identS "let"
                                                                        then POk (inp,inp0) T_less
                                                                        else POk (inp,inp') (T_open_tag tag) ;
                                                   Nothing         -> PFailed pos "matching the html tag failed"
                                                 } ;
                                         _    -> POk (inp,inp0) T_less
                                        } }
\< \/ $white* @ident            { \lang inp@(AI pos s) inp' _ ->
                                     case lang of {
                                       NLG -> case getTag (AI (alexMove (alexMove pos '<') '/') (BS.drop 2 s)) of {
                                                Just (tag,inp') -> POk (inp,inp') (T_close_tag tag) ;
                                                Nothing         -> PFailed pos "matching the html tag failed"
                                              } ;
                                       _   -> let inp0 = AI (alexMove pos '<') (BS.tail s)
                                              in POk (inp,inp0) T_less
                                     } }
\' ([. # [\' \\ \n]] | (\\ (\' | \\)))+ \' { tok (T_Ident . identS . unescapeInitTail . unpack) }
@ident   { tok ident }

\" ([$u # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t | $d+)))* \" { tok (T_String . unescapeInitTail . unpack) }

(\-)? $d+                       { tok (T_Integer . read . unpack) }
(\-)? $d+ \. $d+ (e (\-)? $d+)? { tok (T_Double  . read . unpack) }


{
unpack = UTF8.toString

ident = res T_Ident . identC . rawIdentC

tok f _ inp@(AI _ s) inp' len = POk (inp,inp') (f (UTF8.take len s))

data Token
 = T_exclmark
 | T_patt
 | T_int_label
 | T_oparen
 | T_cparen
 | T_tilde
 | T_star
 | T_starstar
 | T_plus
 | T_plusplus
 | T_comma
 | T_minus
 | T_rarrow
 | T_dot
 | T_alt
 | T_colon
 | T_semicolon
 | T_less
 | T_equal
 | T_big_rarrow
 | T_great
 | T_questmark
 | T_obrack
 | T_lam
 | T_lamlam
 | T_cbrack
 | T_ocurly
 | T_bar
 | T_ccurly
 | T_underscore
 | T_at
 | T_cfarrow
 | T_PType
 | T_Str
 | T_Strs
 | T_Tok
 | T_Type
 | T_abstract
 | T_case
 | T_cat
 | T_concrete
 | T_data
 | T_def
 | T_flags
 | T_fn
 | T_fun
 | T_in
 | T_incomplete
 | T_instance
 | T_interface
 | T_let
 | T_lin
 | T_lincat
 | T_lindef
 | T_linref
 | T_of
 | T_open
 | T_oper
 | T_param
 | T_pattern
 | T_pre
 | T_printname
 | T_resource
 | T_strs
 | T_table
 | T_transfer
 | T_variants
 | T_where
 | T_with
 | T_coercions
 | T_terminator
 | T_separator
 | T_nonempty
 | T_String  String          -- string literals
 | T_Integer Integer         -- integer literals
 | T_Double  Double          -- double precision float literals
 | T_Ident   Ident
 | T_open_tag  Ident
 | T_close_tag Ident
 | T_EOF
 deriving Show -- debug

res = eitherResIdent
eitherResIdent :: (Ident -> Token) -> Ident -> Token
eitherResIdent tv s =
  case Map.lookup s resWords of
    Just t  -> t
    Nothing -> tv s

isReservedWord :: Ident -> Bool
isReservedWord ident = Map.member ident resWords

resWords = Map.fromList
 [ b "!"  T_exclmark
 , b "#"  T_patt
 , b "$"  T_int_label
 , b "("  T_oparen
 , b ")"  T_cparen
 , b "~"  T_tilde
 , b "*"  T_star
 , b "**" T_starstar
 , b "+"  T_plus
 , b "++" T_plusplus
 , b ","  T_comma
 , b "-"  T_minus
 , b "->" T_rarrow
 , b "."  T_dot
 , b "/"  T_alt
 , b ":"  T_colon
 , b ";"  T_semicolon
 , b "="  T_equal
 , b "=>" T_big_rarrow
 , b "<"  T_less
 , b ">"  T_great
 , b "?"  T_questmark
 , b "["  T_obrack
 , b "]"  T_cbrack
 , b "\\" T_lam
 , b "\\\\" T_lamlam
 , b "{"  T_ocurly
 , b "}"  T_ccurly
 , b "|"  T_bar
 , b "_"  T_underscore
 , b "@"  T_at
 , b "::=" T_cfarrow
 , b ":="  T_cfarrow
 , b "PType"      T_PType
 , b "Str"        T_Str
 , b "Strs"       T_Strs
 , b "Tok"        T_Tok
 , b "Type"       T_Type
 , b "abstract"   T_abstract
 , b "case"       T_case
 , b "cat"        T_cat
 , b "concrete"   T_concrete
 , b "data"       T_data
 , b "def"        T_def
 , b "flags"      T_flags
 , b "fn"         T_fn
 , b "fun"        T_fun
 , b "in"         T_in
 , b "incomplete" T_incomplete
 , b "instance"   T_instance
 , b "interface"  T_interface
 , b "let"        T_let
 , b "lin"        T_lin
 , b "lincat"     T_lincat
 , b "lindef"     T_lindef
 , b "linref"     T_linref
 , b "of"         T_of
 , b "open"       T_open
 , b "oper"       T_oper
 , b "param"      T_param
 , b "pattern"    T_pattern
 , b "pre"        T_pre
 , b "printname"  T_printname
 , b "resource"   T_resource
 , b "strs"       T_strs
 , b "table"      T_table
 , b "transfer"   T_transfer
 , b "variants"   T_variants
 , b "where"      T_where
 , b "with"       T_with
 , b "coercions"  T_coercions
 , b "terminator" T_terminator
 , b "separator"  T_separator
 , b "nonempty"   T_nonempty
 ]
 where b s t = (identS s, t)

unescapeInitTail :: String -> String
unescapeInitTail = unesc . tail where
  unesc s = case s of
    []         -> []
    '\"':[]    -> []
    '\'':[]    -> []
    _          -> case readLitChar s of
                    [(c,cs)] -> c:unesc cs

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
            deriving (Eq,Show)

alexMove :: Posn -> Char -> Posn
alexMove (Pn l c) '\n' = Pn (l+1) 1
alexMove (Pn l c) _    = Pn l     (c+1)

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI p s) =
  case WBS.uncons s of
    Nothing  -> Nothing
    Just (w,s) ->
             let p' = alexMove p (BS.w2c w)
             in p' `seq` Just (w, (AI p' s))

data AlexInput = AI {-# UNPACK #-} !Posn            -- current position,
                    {-# UNPACK #-} !BS.ByteString   -- current input string

type AlexInput2 = (AlexInput,AlexInput)

data ParseResult a
  = POk AlexInput2 a
  | PFailed Posn        -- The position of the error
            String      -- The error message

data Lang = GF | BNFC | NLG

newtype P a = P { unP :: Lang -> AlexInput2 -> ParseResult a }

instance Functor P where
  fmap = liftA

instance Applicative P where
  pure = return
  (<*>) = ap

instance Monad P where
  return a    = a `seq` (P $ \_ s -> POk s a)
  (P m) >>= k = P $ \l s -> case m l s of
                              POk s a          -> unP (k a) l s
                              PFailed posn err -> PFailed posn err

#if !(MIN_VERSION_base(4,13,0))
  -- Monad(fail) will be removed in GHC 8.8+
  fail = Fail.fail
#endif

instance Fail.MonadFail P where
  fail msg    = P $ \_ (_,AI posn _) -> PFailed posn msg


runP :: P a -> BS.ByteString -> Either (Posn,String) a
runP p bs = snd <$> runP' GF p (Pn 1 0,bs)

runLangP :: Lang -> P a -> BS.ByteString -> Either (Posn,String) a
runLangP lang p bs = snd <$> runP' lang p (Pn 1 0,bs)

runPartial p s = conv <$> runP' GF p (Pn 1 0,UTF8.fromString s)
  where conv ((pos,rest),x) = (UTF8.toString rest,x)

runP' lang (P f) (pos,txt) =
  case f lang (dup (AI pos txt)) of
    POk (AI pos rest,_) x  -> Right ((pos,rest),x)
    PFailed pos msg -> Left  (pos,msg)

dup x = (x,x)

failLoc :: Posn -> String -> P a
failLoc pos msg = P $ \_ _ -> PFailed pos msg

lexer :: (Token -> P a) -> P a
lexer cont = cont=<<token

token :: P Token
token = P go
  where
    go lang (_,inp) =
      case alexScan inp 0 of
        AlexEOF                -> POk (inp,inp) T_EOF
        AlexError (AI pos _)   -> PFailed pos "lexical error"
        AlexSkip  inp' len     -> go lang (inp,inp')
        AlexToken inp' len act -> act lang inp inp' len

getTag inp = space inp
  where
    space inp = do
      (w,inp') <- alexGetByte inp
      if isSpace (BS.w2c w)
        then space inp'
        else ident [] inp

    ident cs inp = do
      (w,inp') <- alexGetByte inp
      let c = BS.w2c w
      if isAlphaNum c || c == '_'
        then ident (c:cs) inp'
        else return (identS (reverse cs),inp)

getPosn :: P Posn
getPosn = P $ \_ ai2@(_,inp@(AI pos _)) -> POk ai2 pos

}
