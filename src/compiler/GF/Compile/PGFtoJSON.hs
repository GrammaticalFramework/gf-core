module GF.Compile.PGFtoJSON (pgf2json) where

import PGF (showCId)
import qualified PGF.Internal as M
import PGF.Internal (
  Abstr,
  CId,
  CncCat(..),
  CncFun(..),
  Concr,
  DotPos,
  Equation(..),
  Literal(..),
  PArg(..),
  PGF,
  Production(..),
  Symbol(..),
  Type,
  absname,
  abstract,
  cflags,
  cnccats,
  cncfuns,
  concretes,
  funs,
  productions,
  sequences,
  totalCats
  )

import qualified Text.JSON as JSON
import Text.JSON (JSValue(..))

import qualified Data.Array.IArray as Array
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

pgf2json :: PGF -> String
pgf2json pgf =
  JSON.encode $ JSON.makeObj
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
    [ ("name", mkJSStr name)
    , ("startcat", mkJSStr start)
    , ("funs", JSON.makeObj $ map absdef2json (Map.assocs (funs ds)))
    ]

absdef2json :: (CId,(Type,Int,Maybe ([Equation],[[M.Instr]]),Double)) -> (String,JSValue)
absdef2json (f,(typ,_,_,_)) = (showCId f,sig)
  where
    (args,cat) = M.catSkeleton typ
    sig = JSON.makeObj
      [ ("args", JSArray $ map (mkJSStr.showCId) args)
      , ("cat", mkJSStr $ showCId cat)
      ]

lit2json :: Literal -> JSValue
lit2json (LStr s) = mkJSStr s
lit2json (LInt n) = mkJSInt n
lit2json (LFlt d) = JSRational True (toRational d)

concrete2json :: (CId,Concr) -> (String,JSValue)
concrete2json (c,cnc) = (showCId c,obj)
  where
    obj = JSON.makeObj
      [ ("flags", JSON.makeObj [ (showCId k, lit2json v) | (k,v) <- Map.toList (cflags cnc) ])
      , ("productions", JSON.makeObj [ (show cat, JSArray (map frule2json (Set.toList set))) | (cat,set) <- IntMap.toList (productions cnc)])
      , ("functions", JSArray (map ffun2json (Array.elems (cncfuns cnc))))
      , ("sequences", JSArray (map seq2json (Array.elems (sequences cnc))))
      , ("categories", JSON.makeObj $ map cats2json (Map.assocs (cnccats cnc)))
      , ("totalfids", mkJSInt (totalCats cnc))
      ]

cats2json :: (CId, CncCat) -> (String,JSValue)
cats2json (c,CncCat start end _) = (showCId c, ixs)
  where
    ixs = JSON.makeObj
      [ ("start", mkJSInt start)
      , ("end", mkJSInt end)
      ]

frule2json :: Production -> JSValue
frule2json (PApply fid args) =
  JSON.makeObj
    [ ("type", mkJSStr "Apply")
    , ("fid", mkJSInt fid)
    , ("args", JSArray (map farg2json args))
    ]
frule2json (PCoerce arg) =
  JSON.makeObj
    [ ("type", mkJSStr "Coerce")
    , ("arg", mkJSInt arg)
    ]

farg2json :: PArg -> JSValue
farg2json (PArg hypos fid) =
  JSON.makeObj
    [ ("type", mkJSStr "PArg")
    , ("hypos", JSArray $ map (mkJSInt . snd) hypos)
    , ("fid", mkJSInt fid)
    ]

ffun2json :: CncFun -> JSValue
ffun2json (CncFun f lins) =
  JSON.makeObj
    [ ("name", mkJSStr $ showCId f)
    , ("lins", JSArray (map mkJSInt (Array.elems lins)))
    ]

seq2json :: Array.Array DotPos Symbol -> JSValue
seq2json seq = JSArray [sym2json s | s <- Array.elems seq]

sym2json :: Symbol -> JSValue
sym2json (SymCat n l)    = new "SymCat" [mkJSInt n, mkJSInt l]
sym2json (SymLit n l)    = new "SymLit" [mkJSInt n, mkJSInt l]
sym2json (SymVar n l)    = new "SymVar" [mkJSInt n, mkJSInt l]
sym2json (SymKS t)       = new "SymKS"  [mkJSStr t]
sym2json (SymKP ts alts) = new "SymKP"  [JSArray (map sym2json ts), JSArray (map alt2json alts)]
sym2json SymBIND         = new "SymKS"  [mkJSStr "&+"]
sym2json SymSOFT_BIND    = new "SymKS"  [mkJSStr "&+"]
sym2json SymSOFT_SPACE   = new "SymKS"  [mkJSStr "&+"]
sym2json SymCAPIT        = new "SymKS"  [mkJSStr "&|"]
sym2json SymALL_CAPIT    = new "SymKS"  [mkJSStr "&|"]
sym2json SymNE           = new "SymNE"  []

alt2json :: ([Symbol],[String]) -> JSValue
alt2json (ps,ts) = new "Alt" [JSArray (map sym2json ps), JSArray (map mkJSStr ts)]

new :: String -> [JSValue] -> JSValue
new f xs =
  JSON.makeObj
    [ ("type", mkJSStr f)
    , ("args", JSArray xs)
    ]

-- | Make JSON value from string
mkJSStr :: String -> JSValue
mkJSStr = JSString . JSON.toJSString

-- | Make JSON value from integer
mkJSInt :: Integral a => a -> JSValue
mkJSInt = JSRational False . toRational
