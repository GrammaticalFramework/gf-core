-- | Translate concrete syntax to Haskell
module GF.Compile.ConcreteToHaskell(concretes2haskell,concrete2haskell) where

import PGF2(Literal(..))
import Data.List(isPrefixOf,sort,sortOn)
import qualified Data.Map as Map
import GF.Text.Pretty
import GF.Grammar.Predef
import GF.Grammar.Grammar
import GF.Grammar.Macros
import GF.Infra.Ident
import GF.Infra.Option
import GF.Haskell as H
import GF.Compile.GrammarToCanonical

-- | Generate Haskell code for the all concrete syntaxes associated with
-- the named abstract syntax in given the grammar.
concretes2haskell opts absname gr = do
  gr <- grammar2canonical opts absname gr
  let abstr:concrs = modules gr
  return [(filename,render80 $ concrete2haskell opts abstr concr)
             | concr@(MN mn,_) <- concrs,
               let filename = showIdent mn ++ ".hs" :: FilePath
         ]

-- | Generate Haskell code for the given concrete module.
-- The only options that make a difference are
-- @-haskell=noprefix@ and @-haskell=variants@.
concrete2haskell opts abstr@(absname,_) concr@(cncname,mi) =
  haskPreamble absname cncname $$
  vcat (
    nl:Comment "--- Parameter types ---":
    [paramDef id ps | (id,ResParam (Just (L _ ps)) _) <- Map.toList (jments mi)] ++
    nl:Comment "--- Type signatures for linearization functions ---":
    [signature id | (id,CncCat _ _ _ _ _) <- Map.toList (jments mi)] ++
    nl:Comment "--- Linearization types ---":
    [lincatDef id ty | (id,CncCat (Just (L _ ty)) _ _ _ _) <- Map.toList (jments mi)] ++
    nl:Comment "--- Linearization functions ---":
    concat (Map.elems lindefs) ++
    nl:Comment "--- Type classes for projection functions ---":
    -- map labelClass (S.toList labels) ++
    nl:Comment "--- Record types ---":
    [] -- concatMap recordType recs
    )
  where
    nl = Comment ""

    signature c = TypeSig lf (Fun abs (pure lin))
      where
        abs = tcon0 (prefixIdent "A." (gId c))
        lin = tcon0 lc
        lf = linfunName c
        lc = lincatName c

    gId :: Ident -> Ident
    gId = (if haskellOption opts HaskellNoPrefix then id else prefixIdent "G")

    va = haskellOption opts HaskellVariants
    pure = if va then ListT else id

    haskPreamble :: ModuleName -> ModuleName -> Doc
    haskPreamble absname cncname =
      "{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, LambdaCase #-}" $$
      "module" <+> cncname <+> "where" $$
      "import Prelude hiding (Ordering(..))" $$
      "import Control.Applicative((<$>),(<*>))" $$
      "import qualified" <+> absname <+> "as A" $$
      "" $$
      "-- | Token sequences, output form linearization functions" $$
      "type Str = [Tok] -- token sequence" $$
      "" $$
      "-- | Tokens" $$
      "data Tok = TK String | TP [([Prefix],Str)] Str | BIND | SOFT_BIND | SOFT_SPACE | CAPIT | ALL_CAPIT" $$
      "         deriving (Eq,Ord,Show)" $$
      "" $$
      "--- Standard definitions ---" $$
      "linString (A.GString s) ="<+>pure "R_s [TK s]" $$
      "linInt (A.GInt i) ="<+>pure "R_s [TK (show i)]" $$
      "linFloat (A.GFloat x) ="<+>pure "R_s [TK (show x)]" $$
      "" $$
      "----------------------------------------------------" $$
      "-- Automatic translation from GF to Haskell follows" $$
      "----------------------------------------------------"
      where
        pure = if va then brackets else pp

    paramDef id pvs = Data (conap0 (gId id)) (map paramCon pvs) derive
      where
        paramCon (id,ctxt) = ConAp (gId id) [tcon0 (gId cat) | (_,_,QC (_,cat)) <- ctxt]
        derive = ["Eq","Ord","Show"]

    convLinType (Sort s)
      | s == cStr = tcon0 (identS "Str")
    convLinType (QC (_,p)) = tcon0 (gId p)
    convLinType (RecType lbls) = tcon (rcon' ls) (map convLinType ts)
      where (ls,ts) = unzip $ sortOn fst lbls
    convLinType (Table pt lt) = Fun (convLinType pt) (convLinType lt)

    lincatDef c ty = tsyn0 (lincatName c) (convLinType ty)

    lindefs =
      Map.fromListWith (++) 
        [linDef id absctx cat lincat rhs |
            (id,CncFun (Just (absctx,cat,_,lincat)) (Just (L _ rhs)) _ _) <- Map.toList (jments mi)]

    linDef f absctx cat lincat rhs0 =
      (cat,[Eqn (linfunName cat,lhs) rhs'])
      where
        lhs = [ConP (aId f) (map VarP abs_args)]
        aId f = prefixIdent "A." (gId f)

        --[C.Type absctx (TypeApp cat _)] = [t | FunDef f' t<-funs, f'==f]
        (xs,rhs) = termFormCnc rhs0

        abs_args = map abs_arg args
        abs_arg = prefixIdent "abs_"
        args = map (prefixIdent "g" . snd) xs

        rhs' = lets (zipWith letlin args absctx)
                    (convert rhs)
          where
            vs = [(x,a)|((_,x),a)<-zip xs args]

        letlin a acat =
          (a,Ap (Var (linfunName acat)) (Var (abs_arg a)))

    convert (Vr v) = Var (gId v)
    convert (EInt n) = lit n
    convert (EFloat d) = lit d
    convert (K s) = single (Const "TK" `Ap` lit s)
    convert Empty = List []
    convert (App t1 t2) = Ap (convert t1) (convert t2)
    convert (R lbls) = aps (rcon ls) (map (convert.snd) ts)
                         where (ls,ts) = unzip (sortOn fst lbls)
    convert (P t lbl) = ap (proj lbl) (convert t)
    convert (ExtR t1 t2) = Const "ExtR" -- TODO
    convert (T _ cs) = LambdaCase (map ppCase cs)
      where
        ppCase (p,t) = (convertPatt p,convert t)
    convert (V _ ts) = Const "V" -- TODO
    convert (S t p)
      | va        = select_va (convert t) (convert p)
      | otherwise = Ap (convert t) (convert p)
      where
        select_va (List [t]) (List [p]) = Op t "!" p
        select_va (List [t]) p = Op t "!$" p
        select_va t p = Op t "!*" p
    convert (Q  (_,id)) = single (Var id)
    convert (QC (_,id)) = single (Var id)
    convert (C t1 t2)
      | va        = concat_va (convert t1) (convert t2)
      | otherwise = plusplus  (convert t1) (convert t2)
      where
        concat_va (List [List ts1]) (List [List ts2]) = List [List (ts1++ts2)]
        concat_va t1 t2 = Op t1 "+++" t2
    convert (Glue t1 t2) = Const "Glue"
    convert (FV ts)
      | va        = join (List (map convert ts))
      | otherwise = case ts of
                      []     -> Const "error" `Ap` Const (show "empty variant")
                      (t:ts) -> convert t
      where
        join (List [x]) = x
        join x = Const "concat" `Ap` x
    convert (Alts def alts) = single (Const "TP" `Ap` List (map convAlt alts) `Ap` convert def)
      where
        convAlt (t1,t2) = Pair (convert t1) (convert t2)
    convert (Strs ss) = List (map lit ss)
    convert t = error (show t)

    convertPatt (PC c     ps) = ConP (gId c) (map convertPatt ps)
    convertPatt (PP (_,c) ps) = ConP (gId c) (map convertPatt ps)
    convertPatt (PV v)        = VarP v
    convertPatt PW            = WildP
    convertPatt (PR lbls)     = ConP (rcon' ls) (map convertPatt ps)
      where (ls,ps) = unzip $ sortOn fst lbls
    convertPatt (PString s)   = Lit s
    convertPatt (PT _ p)      = convertPatt p
    convertPatt (PAs v p)     = AsP v (convertPatt p)
    convertPatt (PImplArg p)  = convertPatt p
    convertPatt (PTilde _)    = WildP
    convertPatt (PAlt _ _)    = WildP -- TODO
    convertPatt p = error (show p)

    lit s = Const (show s) -- hmm

    ap = if va then ap' else Ap
       where
         ap' (List [f]) x = fmap f x
         ap' f x = Op f "<*>" x
         fmap f (List [x]) = Ap f x
         fmap f x = Op f "<$>" x

    aps f [] = f
    aps f (a:as) = aps (ap f a) as

proj = Var . identS . proj'
proj' (LIdent l) = "proj_" ++ showRawIdent l
rcon = Var . rcon'
rcon' = identS . rcon_name
rcon_name ls = "R"++concat (sort ['_':showRawIdent l | LIdent l <- ls])

lincatName,linfunName :: Ident -> Ident
lincatName c = prefixIdent "Lin" c
linfunName c = prefixIdent "lin" c
