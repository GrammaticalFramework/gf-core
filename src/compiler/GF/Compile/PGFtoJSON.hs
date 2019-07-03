module GF.Compile.PGFtoJSON (pgf2json) where

import PGF(showCId)
import PGF.Internal as M

import qualified Text.JSON as JSON
import Text.JSON (JSValue(..))
-- import Text.JSON.Pretty (pp_value)
-- import Text.PrettyPrint (render)

--import GF.Data.ErrM
--import GF.Infra.Option

--import Control.Monad (mplus)
--import Data.Array.Unboxed (UArray)
import qualified Data.Array.IArray as Array
--import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

pgf2json :: PGF -> String
pgf2json pgf =
  JSON.encode $ JSON.makeObj
  -- render $ pp_value $ JSON.makeObj
    [ ("abstract", json_abstract)
    , ("concretes", json_concretes)
    ]
 where
   n  = showCId $ absname pgf
   as = abstract pgf
   cs = Map.assocs (concretes pgf)
   start = showCId $ M.lookStartCat pgf
   json_abstract = abstract2json n start as
   json_concretes = JSON.makeObj $ map concrete2json cs

abstract2json :: String -> String -> Abstr -> JSValue
abstract2json name start ds =
  JSON.makeObj
    [ ("name", JSString $ JSON.toJSString name)
    , ("startcat", JSString $ JSON.toJSString start)
    , ("funs", JSON.makeObj $ map absdef2js (Map.assocs (funs ds)))
    ]

-- abstract2js :: String -> Abstr -> JS.Expr
-- abstract2js start ds = new "GFAbstract" [JS.EStr start, JS.EObj $ map absdef2js (Map.assocs (funs ds))]

absdef2js :: (CId,(Type,Int,Maybe ([Equation],[[M.Instr]]),Double)) -> (String,JSValue)
absdef2js (f,(typ,_,_,_)) = (showCId f,sig)
  where
    (args,cat) = M.catSkeleton typ
    sig = JSON.makeObj
      [ ("args", JSArray $ map (mkJSString.showCId) args)
      , ("cat", mkJSString $ showCId cat)
      ]

mkJSString :: String -> JSValue
mkJSString = JSString . JSON.toJSString

-- lit2js (LStr s) = JS.EStr s
-- lit2js (LInt n) = JS.EInt n
-- lit2js (LFlt d) = JS.EDbl d

concrete2json :: (CId,Concr) -> (String,JSValue)
concrete2json (c,cnc) = (showCId c,JSNull)

-- concrete2js :: (CId,Concr) -> JS.Property
-- concrete2js (c,cnc) =
--   JS.Prop l (new "GFConcrete" [mapToJSObj (lit2js) $ cflags cnc,
--                                JS.EObj $ [JS.Prop (JS.IntPropName cat) (JS.EArray (map frule2js (Set.toList set))) | (cat,set) <- IntMap.toList (productions cnc)],
--                                JS.EArray $ (map ffun2js (Array.elems (cncfuns cnc))),
--                                JS.EArray $ (map seq2js (Array.elems (sequences cnc))),
--                                JS.EObj $ map cats (Map.assocs (cnccats cnc)),
--                                JS.EInt (totalCats cnc)])
--   where
--    l  = JS.IdentPropName (JS.Ident (showCId c))
-- {-
--    litslins = [JS.Prop (JS.StringPropName    "Int") (JS.EFun [children] [JS.SReturn $ new "Arr" [JS.EIndex (JS.EVar children) (JS.EInt 0)]]),
--                JS.Prop (JS.StringPropName  "Float") (JS.EFun [children] [JS.SReturn $ new "Arr" [JS.EIndex (JS.EVar children) (JS.EInt 0)]]),
--                JS.Prop (JS.StringPropName "String") (JS.EFun [children] [JS.SReturn $ new "Arr" [JS.EIndex (JS.EVar children) (JS.EInt 0)]])]
-- -}
--    cats (c,CncCat start end _) = JS.Prop (JS.IdentPropName (JS.Ident (showCId c))) (JS.EObj [JS.Prop (JS.IdentPropName (JS.Ident "s")) (JS.EInt start)
--                                                                                             ,JS.Prop (JS.IdentPropName (JS.Ident "e")) (JS.EInt end)])
-- {-
-- mkStr :: String -> JS.Expr
-- mkStr s = new "Str" [JS.EStr s]
-- 
-- mkSeq :: [JS.Expr] -> JS.Expr
-- mkSeq [x] = x
-- mkSeq xs = new "Seq" xs
-- 
-- argIdent :: Integer -> JS.Ident
-- argIdent n = JS.Ident ("x" ++ show n)
-- -}
-- children :: JS.Ident
-- children = JS.Ident "cs"
-- 
-- frule2js :: Production -> JS.Expr
-- frule2js (PApply funid args) = new "Apply"  [JS.EInt funid, JS.EArray (map farg2js args)]
-- frule2js (PCoerce arg)       = new "Coerce" [JS.EInt arg]
-- 
-- farg2js (PArg hypos fid) = new "PArg" (map (JS.EInt . snd) hypos ++ [JS.EInt fid])
-- 
-- ffun2js (CncFun f lins) = new "CncFun" [JS.EStr (showCId f), JS.EArray (map JS.EInt (Array.elems lins))]
-- 
-- seq2js :: Array.Array DotPos Symbol -> JS.Expr
-- seq2js seq = JS.EArray [sym2js s | s <- Array.elems seq]
-- 
-- sym2js :: Symbol -> JS.Expr
-- sym2js (SymCat n l)    = new "SymCat" [JS.EInt n, JS.EInt l]
-- sym2js (SymLit n l)    = new "SymLit" [JS.EInt n, JS.EInt l]
-- sym2js (SymVar n l)    = new "SymVar" [JS.EInt n, JS.EInt l]
-- sym2js (SymKS t)       = new "SymKS"  [JS.EStr t]
-- sym2js (SymKP ts alts) = new "SymKP"  [JS.EArray (map sym2js ts), JS.EArray (map alt2js alts)]
-- sym2js SymBIND         = new "SymKS"  [JS.EStr "&+"]
-- sym2js SymSOFT_BIND    = new "SymKS"  [JS.EStr "&+"]
-- sym2js SymSOFT_SPACE   = new "SymKS"  [JS.EStr "&+"]
-- sym2js SymCAPIT        = new "SymKS"  [JS.EStr "&|"]
-- sym2js SymALL_CAPIT    = new "SymKS"  [JS.EStr "&|"]
-- sym2js SymNE           = new "SymNE"  []
-- 
-- alt2js (ps,ts) = new "Alt" [JS.EArray (map sym2js ps), JS.EArray (map JS.EStr ts)]
-- 
-- new :: String -> [JS.Expr] -> JS.Expr
-- new f xs = JS.ENew (JS.Ident f) xs
-- 
-- mapToJSObj :: (a -> JS.Expr) -> Map CId a -> JS.Expr
-- mapToJSObj f m = JS.EObj [ JS.Prop (JS.IdentPropName (JS.Ident (showCId k))) (f v) | (k,v) <- Map.toList m ]
