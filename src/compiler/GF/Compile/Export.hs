module GF.Compile.Export where

import PGF2
import GF.Compile.PGFtoHaskell
--import GF.Compile.PGFtoAbstract
import GF.Compile.PGFtoJava
import GF.Compile.PGFtoJSON
import GF.Infra.Option
--import GF.Speech.CFG
import GF.Speech.PGFToCFG
import GF.Speech.SRGS_ABNF
import GF.Speech.SRGS_XML
import GF.Speech.JSGF
import GF.Speech.GSL
import GF.Speech.SRG
import GF.Speech.VoiceXML
import GF.Speech.SLF
import GF.Speech.PrRegExp

import Data.Maybe
import qualified Data.Map as Map
import System.FilePath
import GF.Text.Pretty

-- top-level access to code generation

-- | Export a PGF to the given 'OutputFormat'. For many output formats,
-- additional 'Options' can be used to control the output.
exportPGF :: Options
          -> OutputFormat 
          -> PGF 
          -> [(FilePath,String)] -- ^ List of recommended file names and contents.
exportPGF opts fmt pgf = 
    case fmt of
      FmtPGFPretty    -> multi "txt" (showPGF)
      FmtCanonicalGF  -> [] -- canon "gf" (render80 . abstract2canonical)
      FmtCanonicalJson-> []
      FmtJSON         -> multi "json"  pgf2json
      FmtHaskell      -> multi "hs"  (grammar2haskell opts name)
      FmtJava         -> multi "java" (grammar2java opts name)
      FmtBNF          -> single "bnf"   bnfPrinter
      FmtEBNF         -> single "ebnf"  (ebnfPrinter opts)
      FmtSRGS_XML     -> single "grxml" (srgsXmlPrinter opts)
      FmtSRGS_XML_NonRec -> single "grxml" (srgsXmlNonRecursivePrinter opts)
      FmtSRGS_ABNF    -> single "gram" (srgsAbnfPrinter opts)
      FmtSRGS_ABNF_NonRec -> single "gram" (srgsAbnfNonRecursivePrinter opts)
      FmtJSGF         -> single "jsgf"  (jsgfPrinter opts)
      FmtGSL          -> single "gsl"   (gslPrinter opts)
      FmtVoiceXML     -> single "vxml"  grammar2vxml
      FmtSLF          -> single "slf"  slfPrinter
      FmtRegExp       -> single "rexp" regexpPrinter
      FmtFA           -> single "dot"  slfGraphvizPrinter
 where
   name = fromMaybe (abstractName pgf) (flag optName opts)

   multi :: String -> (PGF -> String) -> [(FilePath,String)]
   multi ext pr = [(name <.> ext, pr pgf)]

-- canon ext pr = [("canonical"</>name<.>ext,pr pgf)]

   single :: String -> (PGF -> Concr -> String) -> [(FilePath,String)]
   single ext pr = [(concreteName cnc <.> ext, pr pgf cnc) | cnc <- Map.elems (languages pgf)]

