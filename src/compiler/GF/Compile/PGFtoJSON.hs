module GF.Compile.PGFtoJSON (pgf2json) where

import PGF2
import PGF2.Internal
import Text.JSON
import qualified Data.Map as Map

pgf2json :: PGF -> String
pgf2json pgf =
  encode $ makeObj
    [ ("abstract", abstract2json pgf)
    , ("concretes", makeObj $ map concrete2json
                                (Map.toList (languages pgf)))
    ]

abstract2json :: PGF -> JSValue
abstract2json pgf =
  makeObj
    [ ("name", showJSON (abstractName pgf))
    , ("startcat", showJSON (showType [] (startCat pgf)))
    , ("funs", makeObj $ map (absdef2json pgf) (functions pgf))
    ]

absdef2json :: PGF -> Fun -> (String,JSValue)
absdef2json pgf f = (f,sig)
  where
    Just (hypos,cat,_) = fmap unType (functionType pgf f)
    sig = makeObj
      [ ("args", showJSON $ map (\(_,_,ty) -> showType [] ty) hypos)
      , ("cat", showJSON cat)
      ]

lit2json :: Literal -> JSValue
lit2json (LStr s) = showJSON s
lit2json (LInt n) = showJSON n
lit2json (LFlt d) = showJSON d

concrete2json :: (ConcName,Concr) -> (String,JSValue)
concrete2json (c,cnc) = (c,obj)
  where
    obj = makeObj
      [ ("flags", makeObj [(k, lit2json v) | (k,v) <- concrFlags cnc])
      , ("productions", makeObj [(show fid, showJSON (map frule2json (concrProductions cnc fid))) | (_,start,end,_) <- concrCategories cnc, fid <- [start..end]])
      , ("functions", showJSON [ffun2json funid (concrFunction cnc funid) | funid <- [0..concrTotalFuns cnc-1]])
      , ("sequences", showJSON [seq2json seqid (concrSequence cnc seqid) | seqid <- [0..concrTotalSeqs cnc-1]])
      , ("categories", makeObj $ map cat2json (concrCategories cnc))
      , ("totalfids", showJSON (concrTotalCats cnc))
      ]

cat2json :: (Cat,FId,FId,[String]) -> (String,JSValue)
cat2json (cat,start,end,_) = (cat, ixs)
  where
    ixs = makeObj
      [ ("start", showJSON start)
      , ("end",   showJSON end)
      ]

frule2json :: Production -> JSValue
frule2json (PApply fid args) =
  makeObj
    [ ("type", showJSON "Apply")
    , ("fid",  showJSON fid)
    , ("args", showJSON (map farg2json args))
    ]
frule2json (PCoerce arg) =
  makeObj
    [ ("type", showJSON "Coerce")
    , ("arg",  showJSON arg)
    ]

farg2json :: PArg -> JSValue
farg2json (PArg hypos fid) =
  makeObj
    [ ("type", showJSON "PArg")
    , ("hypos", JSArray $ map (showJSON . snd) hypos)
    , ("fid", showJSON fid)
    ]

ffun2json :: FunId -> (Fun,[SeqId]) -> JSValue
ffun2json funid (fun,seqids) =
  makeObj
    [ ("name", showJSON fun)
    , ("lins", showJSON seqids)
    ]

seq2json :: SeqId -> [Symbol] -> JSValue
seq2json seqid seq = showJSON [sym2json sym | sym <- seq]

sym2json :: Symbol -> JSValue
sym2json (SymCat n l)    = new "SymCat" [showJSON n, showJSON l]
sym2json (SymLit n l)    = new "SymLit" [showJSON n, showJSON l]
sym2json (SymVar n l)    = new "SymVar" [showJSON n, showJSON l]
sym2json (SymKS t)       = new "SymKS"  [showJSON t]
sym2json (SymKP ts alts) = new "SymKP"  [JSArray (map sym2json ts), JSArray (map alt2json alts)]
sym2json SymBIND         = new "SymKS"  [showJSON "&+"]
sym2json SymSOFT_BIND    = new "SymKS"  [showJSON "&+"]
sym2json SymSOFT_SPACE   = new "SymKS"  [showJSON "&+"]
sym2json SymCAPIT        = new "SymKS"  [showJSON "&|"]
sym2json SymALL_CAPIT    = new "SymKS"  [showJSON "&|"]
sym2json SymNE           = new "SymNE"  []

alt2json :: ([Symbol],[String]) -> JSValue
alt2json (ps,ts) = new "Alt" [showJSON (map sym2json ps), showJSON ts]

new :: String -> [JSValue] -> JSValue
new f xs =
  makeObj
    [ ("type", showJSON f)
    , ("args", showJSON xs)
    ]
