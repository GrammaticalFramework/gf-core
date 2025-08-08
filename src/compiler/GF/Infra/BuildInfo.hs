{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module GF.Infra.BuildInfo where
import System.Info
import Data.Version(showVersion)

import Language.Haskell.TH.Syntax
import Control.Monad.IO.Class
import Control.Exception
import Data.Time
import Data.Time.Format.ISO8601
import System.Process

-- Use Template Haskell to get compile time
buildTime :: String
buildTime = $(do
                 timeZone <- liftIO getCurrentTimeZone
                 time <- liftIO $ utcToLocalTime timeZone <$> getCurrentTime
                 return $ LitE $ StringL $ iso8601Show time )

-- Use Template Haskell to get current Git information
gitInfo :: String
gitInfo = $(do
               info <- liftIO $ try $ readProcess "git" ["log", "--format=\"Commit %h Tag %(describe:tags=true)\"", "-1"] "" :: Q (Either SomeException String)
               return $ LitE $ StringL $ either (\_ -> "unavailable") id info )

{-# NOINLINE buildInfo #-}
buildInfo =
    "Built on "++os++"/"++arch
    ++" with "++compilerName++"-"++showVersion compilerVersion ++ " at " ++ buildTime ++ "\nGit info: " ++ gitInfo
    ++"\nFlags:"
#ifdef USE_INTERRUPT
    ++" interrupt"
#endif
#ifdef SERVER_MODE
    ++" server"
#endif
#ifdef NEW_COMP
    ++" new-comp"
#endif
#ifdef C_RUNTIME
    ++" c-runtime"
#endif
