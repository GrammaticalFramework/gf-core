-------------------------------------------------
-- |
-- Module      : PGF2.Colab
-- Maintainer  : Krasimir Angelov
-- Stability   : stable
-- Portability : portable
--
-- This module is a server API for implementing
-- collaborative editing, where the edited document
-- is continuously parsed.
-------------------------------------------------

module PGF2.Collab
         ( ParseChart, parseChart
         , getParseChartText
         , Change(..), changeParseChartText
         ) where

import PGF2.Expr
import PGF2.FFI

import Foreign
import Foreign.C
import Control.Exception(bracket)

#include <pgf/pgf.h>

data ParseChart = ParseChart {ch_db :: Ptr PgfDB,
                              ch_revision :: ForeignPtr Concr,
                              ch_chart :: ForeignPtr PgfParseChart
                             }

parseChart :: Concr -> Type -> String -> IO ParseChart
parseChart c ty sent =
  withForeignPtr (c_revision c) $ \c_revision_ptr ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  withText sent $ \c_sent -> do
    c_chart <- withPgfExn "parseChart" (pgf_parse_chart (c_db c) c_revision_ptr c_ty marshaller unmarshaller c_sent)
    fptr <- newForeignPtr pgf_free_parse_chart c_chart
    return (ParseChart (c_db c) (c_revision c) fptr)

getParseChartText :: ParseChart -> IO String
getParseChartText (ParseChart c_db c_revision c_chart) =
  withForeignPtr c_chart $ \c_chart_ptr -> do
    c_get_text <- (#peek PgfParseChartVtbl, get_text) =<< (#peek PgfParseChart, vtbl) c_chart_ptr
    c_text <- callGetText c_get_text c_chart_ptr
    peekText c_text

data Change   = Skip {-# UNPACK #-} !CSize | Change {-# UNPACK #-} !CSize String deriving Show

changeParseChartText :: ParseChart -> [Change] -> IO Bool
changeParseChartText (ParseChart c_db c_revision c_chart) changes =
  withForeignPtr c_revision $ \_ ->
  withForeignPtr c_chart $ \c_chart_ptr -> do
    c_start <- (#peek PgfParseChartVtbl, start) =<< (#peek PgfParseChart, vtbl) c_chart_ptr
    callStart c_start c_chart_ptr
    res <- apply c_chart_ptr changes 
    c_done <- (#peek PgfParseChartVtbl, done) =<< (#peek PgfParseChart, vtbl) c_chart_ptr
    callDone c_done c_chart_ptr
    return res
  where
    apply c_chart_ptr []                      = return True
    apply c_chart_ptr (Skip   i     :changes) = do
      c_skip <- (#peek PgfParseChartVtbl, skip) =<< (#peek PgfParseChart, vtbl) c_chart_ptr
      c_res  <- callSkip c_skip c_chart_ptr i
      if c_res == 0
        then return False
        else apply c_chart_ptr changes
    apply c_chart_ptr (Change i text:changes) = do
      c_change <- (#peek PgfParseChartVtbl, change) =<< (#peek PgfParseChart, vtbl) c_chart_ptr
      c_res <- withText text (callChange c_change c_chart_ptr i)
      if c_res == 0
        then return False
        else apply c_chart_ptr changes
