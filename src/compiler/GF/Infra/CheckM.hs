----------------------------------------------------------------------
-- |
-- Module      : CheckM
-- Maintainer  : (Maintainer)
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/04/21 16:22:33 $
-- > CVS $Author: bringert $
-- > CVS $Revision: 1.5 $
--
-- (Description of the module)
-----------------------------------------------------------------------------

module GF.Infra.CheckM
          (Check(..), CheckResult(..), Message, runCheck, runCheck',
           checkError, checkCond, checkWarn, checkWarnings, checkAccumError,
           checkIn, checkInModule, checkMap, checkMapRecover,
           accumulateError, commitCheck,
          ) where

import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint
import GF.Data.Operations
--import GF.Infra.Ident
--import GF.Grammar.Grammar(msrc) -- ,Context
import GF.Infra.Location(ppLocation,sourcePath)
import GF.Infra.Option(Options,noOptions,verbAtLeast,Verbosity(..))

import qualified Data.Map as Map
import GF.Text.Pretty
import System.FilePath(makeRelative)
import Control.Parallel.Strategies(parList,rseq,using)
import Control.Monad(liftM,ap)
import Control.Applicative(Applicative(..))
import qualified Control.Monad.Fail as Fail

type Message = Doc
type Error   = Message
type Warning = Message
type NonFatal = ([Error],[Warning])
data CheckResult a b = Fail Error b | Success a b
newtype Check a
  = Check {unCheck :: NonFatal -> CheckResult a NonFatal}

instance Functor Check where fmap = liftM

instance Monad Check where
  return x = Check $ \msgs -> Success x msgs
  f >>= g  = Check $ \ws ->
               case unCheck f ws of
                 Success x msgs -> unCheck (g x) msgs
                 Fail msg msgs  -> Fail msg msgs

instance Fail.MonadFail Check where
  fail = raise

instance Applicative Check where
  pure = return
  (<*>) = ap

instance ErrorMonad Check where
  raise s = checkError (pp s)
  handle f h = handle' f (h . render)

handle' f h = Check (\msgs -> case unCheck f {-ctxt-} msgs of
                                Success x msgs -> Success x msgs
                                Fail msg msgs  -> unCheck (h msg) msgs)

-- | Report a fatal error
checkError :: Message -> Check a
checkError msg = Check (\msgs -> Fail msg msgs)

checkCond :: Message -> Bool -> Check ()
checkCond s b = if b then return () else checkError s

-- | warnings should be reversed in the end
checkWarn :: Message -> Check ()
checkWarn msg = Check $ \(es,ws) -> Success () (es,("Warning:" <+> msg) : ws)

checkWarnings ms = mapM_ checkWarn ms

-- | Report a nonfatal (accumulated) error
checkAccumError :: Message -> Check ()
checkAccumError msg = Check $ \(es,ws) -> Success () (msg:es,ws)

-- | Turn a fatal error into a nonfatal (accumulated) error
accumulateError :: (a -> Check a) -> a -> Check a
accumulateError chk a =
    handle' (chk a) $ \ msg -> do checkAccumError msg; return a

-- |  Turn accumulated errors into a fatal error
commitCheck :: Check a -> Check a
commitCheck c =
    Check $ \msgs0@(es0,ws0) ->
    case unCheck c ([],[]) of
      (Success v ([],ws)) -> Success v (es0,ws++ws0)
      (Success _ msgs)    -> bad msgs0 msgs
      (Fail    e (es,ws)) -> bad msgs0 ((e:es),ws)
  where
    bad (es0,ws0) (es,ws) = (Fail (list es) (es0,ws++ws0))
    list = vcat . reverse

-- | Run an error check, report errors and warnings
runCheck c = runCheck' noOptions c

-- | Run an error check, report errors and (optionally) warnings
runCheck' :: ErrorMonad m => Options -> Check a -> m (a,String)
runCheck' opts c =
    case unCheck c ([],[]) of
      Success v ([],ws) -> return (v,render (wlist ws))
      Success v msgs    -> bad msgs
      Fail    e (es,ws) -> bad ((e:es),ws)
  where
    bad (es,ws) = raise (render $ wlist ws $$ list es)
    list = vcat . reverse
    wlist ws = if verbAtLeast opts Normal then list ws else empty

checkMap :: (Ord a) => (a -> b -> Check b) -> Map.Map a b -> Check (Map.Map a b)
checkMap f map = do xs <- mapM (\(k,v) -> do v <- f k v
                                             return (k,v)) (Map.toList map)
                    return (Map.fromAscList xs)

checkMapRecover :: (Ord a) => (a -> b -> Check b) -> Map.Map a b -> Check (Map.Map a b)
checkMapRecover f = fmap Map.fromList . mapM f' . Map.toList
  where f' (k,v) = fmap ((,)k) (f k v)

checkIn :: Doc -> Check a -> Check a
checkIn msg c = Check $ \msgs0 ->
    case unCheck c ([],[]) of
      Fail msg  msgs -> Fail (augment1 msg) (augment msgs0 msgs)
      Success v msgs -> Success v (augment msgs0 msgs)
  where
    augment (es0,ws0) (es,ws) = (augment' es0 es,augment' ws0 ws)

    augment' msgs0 []    = msgs0
    augment' msgs0 msgs' = (msg $$ nest 3 (vcat (reverse msgs'))):msgs0

    augment1 msg' = msg $$ nest 3 msg'

-- | Augment error messages with a relative path to the source module and
-- an contextual hint (which can be left 'empty')
checkInModule cwd mi loc context =
    checkIn (ppLocation relpath loc <> ':' $$ nest 2 context)
  where
    relpath = makeRelative cwd (sourcePath mi)
