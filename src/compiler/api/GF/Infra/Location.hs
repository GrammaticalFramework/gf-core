-- | Source locations
module GF.Infra.Location where
import Prelude hiding ((<>))
import GF.Text.Pretty

-- ** Source locations

class HasSourcePath a where sourcePath :: a -> FilePath

data Location 
  = NoLoc
  | Local Int Int
  | External FilePath Location
  deriving (Show,Eq,Ord)

-- | Attaching location information
data L a = L Location a deriving (Show, Eq, Ord)

instance Functor L where fmap f (L loc x) = L loc (f x)

instance Foldable L where foldr f b (L loc x) = f x b

instance Traversable L where traverse f (L loc x) = pure (L loc) <*> f x

unLoc :: L a -> a
unLoc (L _ x) = x

noLoc = L NoLoc

ppLocation :: FilePath -> Location -> Doc
ppLocation fpath NoLoc          = pp fpath
ppLocation fpath (External p l) = ppLocation p l
ppLocation fpath (Local b e) =
    opt (fpath/="") (fpath <> ":") <> b <> opt (b/=e) ("-" <> e)
  where
    opt False x = empty
    opt True x = x

ppL (L loc x) msg = hang (loc<>":") 4 ("In"<+>x<>":"<+>msg)


instance Pretty Location where pp = ppLocation ""

instance Pretty a => Pretty (L a) where pp (L loc x) = loc<>":"<>x

