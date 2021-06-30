-- | Translate concrete syntax to Haskell
module GF.Compile.ConcreteToHaskell(concretes2haskell,concrete2haskell) where
import Data.List(isPrefixOf,sort,sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import GF.Text.Pretty
--import GF.Grammar.Predef(cPredef,cInts)
--import GF.Compile.Compute.Predef(predef)
--import GF.Compile.Compute.Value(Predefined(..))
import GF.Infra.Ident(Ident,identC,identS,identW,prefixIdent,showRawIdent,rawIdentS)
import GF.Infra.Option
import GF.Haskell as H
import GF.Grammar.Canonical as C
import GF.Compile.GrammarToCanonical
import Debug.Trace(trace)

-- | Generate Haskell code for the all concrete syntaxes associated with
-- the named abstract syntax in given the grammar.
concretes2haskell opts absname gr =
  [(filename,render80 $ concrete2haskell opts abstr cncmod)
     | let Grammar abstr cncs = grammar2canonical opts absname gr,
       cncmod<-cncs,
       let ModId name = concName cncmod
           filename = showRawIdent name ++ ".hs" :: FilePath
  ]

-- | Generate Haskell code for the given concrete module.
-- The only options that make a difference are
-- @-haskell=noprefix@ and @-haskell=variants@.
concrete2haskell opts
                 abstr@(Abstract _ _ cats funs)
                 modinfo@(Concrete cnc absname _ ps lcs lns) =
  haskPreamble absname cnc $$
  vcat (
    nl:Comment "--- Parameter types ---":
    map paramDef ps ++
    nl:Comment "--- Type signatures for linearization functions ---":
    map signature cats ++
    nl:Comment "--- Linearization functions for empty categories ---":
    emptydefs ++
    nl:Comment "--- Linearization types ---":
    map lincatDef lcs ++
    nl:Comment "--- Linearization functions ---":
    lindefs ++
    nl:Comment "--- Type classes for projection functions ---":
    map labelClass (S.toList labels) ++
    nl:Comment "--- Record types ---":
    concatMap recordType recs)
  where
    nl = Comment ""
    recs = S.toList (S.difference (records (lcs,lns)) common_records)

    labels = S.difference (S.unions (map S.fromList recs)) common_labels
    common_records = S.fromList [[label_s]]
    common_labels = S.fromList [label_s]
    label_s = LabelId (rawIdentS "s")

    signature (CatDef c _) = TypeSig lf (Fun abs (pure lin))
      where
        abs = tcon0 (prefixIdent "A." (gId c))
        lin = tcon0 lc
        lf = linfunName c
        lc = lincatName c

    emptydefs = map emptydef (S.toList emptyCats)
    emptydef c = Eqn (linfunName c,[WildP]) (Const "undefined")

    emptyCats = allcats `S.difference` linfuncats
      where
     --funcats = S.fromList [c | FunDef f (C.Type _ (TypeApp c _))<-funs]
       allcats = S.fromList [c | CatDef c _<-cats]

    gId :: ToIdent i => i -> Ident
    gId = (if haskellOption opts HaskellNoPrefix then id else prefixIdent "G")
          . toIdent

    va = haskellOption opts HaskellVariants
    pure = if va then ListT else id

    haskPreamble :: ModId -> ModId -> Doc
    haskPreamble absname cncname =
      "{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, LambdaCase #-}" $$
      "module" <+> cncname <+> "where" $$
      "import Prelude hiding (Ordering(..))" $$
      "import Control.Applicative((<$>),(<*>))" $$
      "import PGF.Haskell" $$
      "import qualified" <+> absname <+> "as A" $$
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

    paramDef pd =
      case pd of
        ParamAliasDef p t -> H.Type (conap0 (gId p)) (convLinType t)
        ParamDef p pvs -> Data (conap0 (gId p)) (map paramCon pvs) derive
          where
            paramCon (Param c cs) = ConAp (gId c) (map (tcon0.gId) cs)
            derive = ["Eq","Ord","Show"]

    convLinType = ppT
      where
        ppT t =
          case t of
            FloatType -> tcon0 (identS "Float")
            IntType -> tcon0 (identS "Int")
            ParamType (ParamTypeId p) -> tcon0 (gId p)
            RecordType rs -> tcon (rcon' ls) (map ppT ts)
              where (ls,ts) = unzip $ sortOn fst [(l,t)|RecordRow l t<-rs]
            StrType -> tcon0 (identS "Str")
            TableType pt lt -> Fun (ppT pt) (ppT lt)
--          TupleType lts ->

    lincatDef (LincatDef c t) = tsyn0 (lincatName c) (convLinType t)

    linfuncats = S.fromList linfuncatl
    (linfuncatl,lindefs) = unzip (linDefs lns)

    linDefs = map eqn . sortOn fst . map linDef
      where eqn (cat,(f,(ps,rhs))) = (cat,Eqn (f,ps) rhs)

    linDef (LinDef f xs rhs0) =
        (cat,(linfunName cat,(lhs,rhs)))
      where
        lhs = [ConP (aId f) (map VarP abs_args)]
        aId f = prefixIdent "A." (gId f)

        [lincat] = [lincat | LincatDef c lincat<-lcs,c==cat]
        [C.Type absctx (TypeApp cat _)] = [t | FunDef f' t<-funs, f'==f]

        abs_args = map abs_arg args
        abs_arg = prefixIdent "abs_"
        args = map (prefixIdent "g" . toIdent) xs

        rhs = lets (zipWith letlin args absctx)
                   (convert vs (coerce env lincat rhs0))
          where
            vs = [(VarValueId (Unqual x),a)|(VarId x,a)<-zip xs args]
            env= [(VarValueId (Unqual x),lc)|(VarId x,lc)<-zip xs (map arglincat absctx)]

        letlin a (TypeBinding _ (C.Type _ (TypeApp acat _))) =
          (a,Ap (Var (linfunName acat)) (Var (abs_arg a)))

        arglincat (TypeBinding _ (C.Type _ (TypeApp acat _))) = lincat
          where
            [lincat] = [lincat | LincatDef c lincat<-lcs,c==acat]

    convert = convert' va

    convert' va vs = ppT
      where
        ppT0 = convert' False vs
        ppTv vs' = convert' va vs'

        pure = if va then single else id

        ppT t =
          case t of
            TableValue ty cs -> pure (table cs)
            Selection t p -> select (ppT t) (ppT p)
            ConcatValue t1 t2 -> concat (ppT t1) (ppT t2)
            RecordValue r -> aps (rcon ls) (map ppT ts)
              where (ls,ts) = unzip $ sortOn fst [(l,t)|RecordRow l t<-r]
            PredefValue p -> single (Var (toIdent p)) -- hmm
            Projection t l -> ap (proj l) (ppT t)
            VariantValue [] -> empty
            VariantValue ts@(_:_) -> variants ts
            VarValue x -> maybe (Var (gId x)) (pure . Var) $ lookup x vs
            PreValue vs t' -> pure (alts t' vs)
            ParamConstant (Param c vs) -> aps (Var (pId c)) (map ppT vs)
            ErrorValue s -> ap (Const "error") (Const (show s)) -- !!
            LiteralValue l -> ppL l
            _ -> error ("convert "++show t)

        ppL l =
          case l of
            FloatConstant x -> pure (lit x)
            IntConstant n -> pure (lit n)
            StrConstant s -> pure (token s)

        pId p@(ParamId s) =
          if "to_R_" `isPrefixOf` unqual s then toIdent p else gId p -- !! a hack

        table cs =
            if all (null.patVars) ps
            then lets ds (LambdaCase [(ppP p,t')|(p,t')<-zip ps ts'])
            else LambdaCase (map ppCase cs)
          where
            (ds,ts') = dedup ts
            (ps,ts) = unzip [(p,t)|TableRow p t<-cs]
        ppCase (TableRow p t) = (ppP p,ppTv (patVars p++vs) t)
{-
        ppPredef n =
          case predef n of
            Ok BIND       -> single (c "BIND")
            Ok SOFT_BIND  -> single (c "SOFT_BIND")
            Ok SOFT_SPACE -> single (c "SOFT_SPACE")
            Ok CAPIT      -> single (c "CAPIT")
            Ok ALL_CAPIT  -> single (c "ALL_CAPIT")
            _ -> Var n
-}
        ppP p =
          case p of
            ParamPattern (Param c ps) -> ConP (gId c) (map ppP ps)
            RecordPattern r -> ConP (rcon' ls) (map ppP ps)
              where (ls,ps) = unzip $ sortOn fst [(l,p)|RecordRow l p<-r]
            WildPattern -> WildP

        token s = single (c "TK" `Ap` lit s)

        alts t' vs = single (c "TP" `Ap` List (map alt vs) `Ap` ppT0 t')
          where
            alt (s,t) = Pair (List (pre s)) (ppT0 t)
            pre s = map lit s

        c = Const
        lit s = c (show s) -- hmm
        concat = if va then concat' else plusplus
          where
            concat' (List [List ts1]) (List [List ts2]) = List [List (ts1++ts2)]
            concat' t1 t2 = Op t1 "+++" t2

        pure' = single -- forcing the list monad

        select = if va then select' else Ap
        select' (List [t]) (List [p]) = Op t "!" p
        select' (List [t]) p = Op t "!$" p
        select' t p = Op t "!*" p

        ap = if va then ap' else Ap
          where
            ap' (List [f]) x = fmap f x
            ap' f x = Op f "<*>" x
            fmap f (List [x]) = pure' (Ap f x)
            fmap f x = Op f "<$>" x

    --  join = if va then join' else id
        join' (List [x]) = x
        join' x = c "concat" `Ap` x

        empty = if va then List [] else c "error" `Ap` c (show "empty variant")
        variants = if va then \ ts -> join' (List (map ppT ts))
                         else \ (t:_) -> ppT t

        aps f [] = f
        aps f (a:as) = aps (ap f a) as

        dedup ts =
            if M.null dups
            then ([],map ppT ts)
            else ([(ev i,ppT t)|(i,t)<-defs],zipWith entry ts is)
          where
            entry t i = maybe (ppT t) (Var . ev) (M.lookup i dups)
            ev i = identS ("e'"++show i)

            defs = [(i1,t)|(t,i1:_:_)<-ms]
            dups = M.fromList [(i2,i1)|(_,i1:is@(_:_))<-ms,i2<-i1:is]
            ms = M.toList m
            m = fmap sort (M.fromListWith (++) (zip ts [[i]|i<-is]))
            is = [0..]::[Int]


--con = Cn . identS

class Records t where
  records :: t -> S.Set [LabelId]

instance Records t => Records [t] where
  records = S.unions . map records

instance (Records t1,Records t2) => Records (t1,t2) where
  records (t1,t2) = S.union (records t1) (records t2)

instance Records LincatDef where
  records (LincatDef _ lt) = records lt

instance Records LinDef where
  records (LinDef _ _ lv) = records lv

instance Records LinType where
  records t =
    case t of
      RecordType r -> rowRecords r
      TableType pt lt -> records (pt,lt)
      TupleType ts -> records ts
      _ -> S.empty

rowRecords r = S.insert (sort ls) (records ts)
  where (ls,ts) = unzip [(l,t)|RecordRow l t<-r]

instance Records LinValue where
  records v =
    case v of
      ConcatValue v1 v2 -> records (v1,v2)
      ParamConstant (Param c vs) -> records vs
      RecordValue r -> rowRecords r
      TableValue t r -> records (t,r)
      TupleValue vs -> records vs
      VariantValue vs -> records vs
      PreValue alts d -> records (map snd alts,d)
      Projection v l -> records v
      Selection v1 v2 -> records (v1,v2)
      _ -> S.empty

instance Records rhs => Records (TableRow rhs) where
  records (TableRow _ v) = records v


-- | Record subtyping is converted into explicit coercions in Haskell
coerce env ty t =
  case (ty,t) of
    (_,VariantValue ts) -> VariantValue (map (coerce env ty) ts)
    (TableType ti tv,TableValue _ cs) ->
      TableValue ti [TableRow p (coerce env tv t)|TableRow p t<-cs]
    (RecordType rt,RecordValue r) ->
      RecordValue [RecordRow l (coerce env ft f) |
                     RecordRow l f<-r,ft<-[ft | RecordRow l' ft <- rt, l'==l]]
    (RecordType rt,VarValue x)->
      case lookup x env of
        Just ty' | ty'/=ty -> -- better to compare to normal form of ty'
                            --trace ("coerce "++render ty'++" to "++render ty) $
                            app (to_rcon rt) [t]
                 | otherwise -> t -- types match, no coercion needed
        _ -> trace (render ("missing type to coerce"<+>x<+>"to"<+>render ty
                            $$ "in" <+> map fst env))
                   t
    _ -> t
  where
    app f ts = ParamConstant (Param f ts) -- !! a hack
    to_rcon = ParamId . Unqual . rawIdentS . to_rcon' . labels

patVars p = []

labels r = [l | RecordRow l _ <- r]

proj = Var . identS . proj'
proj' (LabelId l) = "proj_" ++ showRawIdent l
rcon = Var . rcon'
rcon' = identS . rcon_name
rcon_name ls = "R"++concat (sort ['_':showRawIdent l | LabelId l <- ls])
to_rcon' = ("to_"++) . rcon_name

recordType ls =
    Data lhs [app] ["Eq","Ord","Show"]:
    enumAllInstance:
    zipWith projection vs ls ++
    [Eqn (identS (to_rcon' ls),[VarP r])
         (foldl Ap (Var cn) [Var (identS (proj' l)) `Ap` Var r|l<-ls])]
  where
    r = identS "r"
    cn = rcon' ls
 -- Not all record labels are syntactically correct as type variables in Haskell
 -- app = cn<+>ls
    lhs = ConAp cn vs -- don't reuse record labels
    app = fmap TId lhs
    tapp = foldl TAp (TId cn) (map TId vs)
    vs = [identS ('t':show i)|i<-[1..n]]
    n = length ls

    projection v l = Instance [] (TId name `TAp` tapp `TAp` TId v)
                              [((prj,[papp]),Var v)]
     where
       name = identS ("Has_"++render l)
       prj = identS (proj' l)
       papp = ConP cn (map VarP vs)

    enumAllInstance =
       Instance ctx (tEnumAll `TAp` tapp)[(lhs0 "enumAll",enumCon cn n)]
      where
        ctx =  [tEnumAll `TAp` TId v|v<-vs]
        tEnumAll = TId (identS "EnumAll")

labelClass l =
    Class [] (ConAp name [r,a]) [([r],[a])]
          [(identS (proj' l),TId r `Fun` TId a)]
  where
    name = identS ("Has_"++render l)
    r = identS "r"
    a = identS "a"

enumCon name arity =
    if arity==0
    then single (Var name)
    else foldl ap (single (Var name)) (replicate arity (Const "enumAll"))
  where
    ap (List [f]) a = Op f "<$>" a
    ap f a = Op f "<*>" a

lincatName,linfunName :: CatId -> Ident
lincatName c = prefixIdent "Lin" (toIdent c)
linfunName c = prefixIdent "lin" (toIdent c)

class ToIdent i where toIdent :: i -> Ident

instance ToIdent ParamId where toIdent (ParamId q) = qIdentC q
instance ToIdent PredefId where toIdent (PredefId s) = identC s
instance ToIdent CatId   where toIdent (CatId s) = identC s
instance ToIdent C.FunId where toIdent (FunId s) = identC s
instance ToIdent VarValueId where toIdent (VarValueId q) = qIdentC q

qIdentC = identS . unqual

unqual (Qual (ModId m) n) = showRawIdent m++"_"++ showRawIdent n
unqual (Unqual n) = showRawIdent n

instance ToIdent VarId where
  toIdent Anonymous = identW
  toIdent (VarId s) = identC s
