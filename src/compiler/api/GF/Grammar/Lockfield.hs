----------------------------------------------------------------------
-- |
-- Module      : Lockfield
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/11/11 23:24:34 $ 
-- > CVS $Author: aarne $
-- > CVS $Revision: 1.7 $
--
-- Creating and using lock fields in reused resource grammars.
--
-- AR 8\/2\/2005 detached from 'compile/MkResource'
-----------------------------------------------------------------------------

module GF.Grammar.Lockfield (lock, lockLabel, isLockLabel) where

import GF.Infra.Ident
import GF.Grammar.Predef
import GF.Grammar.Grammar

import GF.Data.Operations(ErrorMonad,Err(..))

lock :: Ident -> Term -> Term
lock c t@(RecType rs) =
  let lbl = lockLabel c
  in if elem lbl (map fst rs) || elem c [cString,cInt]
       then t --- don't add an extra copy of lock field, nor predef cats
       else RecType (rs ++ [(lbl, RecType [])])
lock c t@(R rs) =
  let lbl = lockLabel c
  in if elem lbl (map fst rs)
       then t
       else R (rs ++ [(lbl, (Just (RecType []),R []))])
lock c (Abs b x t) = Abs b x (lock c t)
lock c (FV ts)     = FV (map (lock c) ts)
lock c t           = t

lockLabel :: Ident -> Label
lockLabel c = LIdent $! prefixRawIdent lockPrefix (ident2raw c)

isLockLabel :: Label -> Maybe RawIdent
isLockLabel l = case l of
  LIdent c -> isPrefixOf lockPrefix c
  _        -> Nothing

lockPrefix = rawIdentS "lock_"
