module GF.Compile.PGFtoJava (grammar2java) where

import PGF2
import Data.Maybe(maybe)
import Data.List(intercalate)
import GF.Infra.Option

-- | the main function
grammar2java :: Options
                -> String  -- ^ Module name.
                -> PGF
                -> String
grammar2java opts name gr = unlines $  
  javaPreamble name ++ methods ++ javaEnding
  where
    methods = [javaMethod gr fun | fun <- functions gr]

javaPreamble name =
 [
  "import org.grammaticalframework.pgf.*;",
  "",
  "public class " ++ name ++ " {",
  ""
 ]
 
javaMethod gr fun =
  "  public static Expr "++fun++"("++arg_decls++") { return new Expr("++show fun++args++"); }"
  where
    arity = maybe 0 getArrity (functionType gr fun)
    vars  = ['e':show i | i <- [1..arity]]
    
    arg_decls = intercalate "," ["Expr "++v | v <- vars]
    args      = if null vars then ",new Expr[] {}" else ","++intercalate "," vars

    getArrity ty = length hypos
      where
        (hypos,_,_) = unType ty

javaEnding =
 [
  "",
  "}"
 ]
