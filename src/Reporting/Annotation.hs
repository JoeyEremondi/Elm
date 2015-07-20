module Reporting.Annotation where

import Prelude hiding (map)

import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R

import qualified Data.List as List
import qualified Language.ECMAScript3.Syntax as JS

-- ANNOTATION

data Annotated annotation a
    = A annotation a
    deriving (Show)


type Located a =
    Annotated R.Region a


type Commented a =
    Annotated (R.Region, Maybe String) a


-- CREATE

at :: R.Position -> R.Position -> a -> Located a
at start end value =
    A (R.Region start end) value


merge :: Located a -> Located b -> value -> Located value
merge (A region1 _) (A region2 _) value =
    A (R.merge region1 region2) value


sameAs :: Annotated info a -> b -> Annotated info b
sameAs (A annotation _) value =
    A annotation value


-- MANIPULATE

map :: (a -> b) -> Annotated info a -> Annotated info b
map f (A annotation value) =
    A annotation (f value)


drop :: Annotated info a -> a
drop (A _ value) =
    value


-- PRETTY PRINT

instance (P.Pretty a) => P.Pretty (Annotated info a) where
  pretty dealiaser parens (A _ value) =
      P.pretty dealiaser parens value


--Data type for annotations of expressions after canonicalization
--We store the region of code that they are in
--As well as information gathered from analyses for optimimzation
data ExprMetaData = ExprMetaData
  { region :: R.Region
  , isTailCallWithArgs :: Maybe (String, [JS.Expression () -> [(String, JS.Expression ())]])
  , hasTailCall :: Maybe String
  , ident :: Int
  }


instance Show ExprMetaData where
  show = show . region


--Create an annotation with no tail call information
defaultMetaData :: R.Region -> ExprMetaData
defaultMetaData reg =
  ExprMetaData
  { region = reg --Where in the source code is this expression?
  , isTailCallWithArgs = Nothing --Is this expression a function containing tail calls?
  , hasTailCall = Nothing --Is this expression a call in tail-position?
  , ident = error "Should not access ident before assigning"
  }


addDefaultAnnot :: Located a -> LocatedWithMeta a
addDefaultAnnot (A reg x) = A (defaultMetaData reg) x


type LocatedWithMeta  a =
  Annotated (ExprMetaData) a


type CommentedWithMeta a =
  Annotated (ExprMetaData, Maybe String) a


--Remove the extra canonical annotation information, leaving just the region
removeMeta :: LocatedWithMeta a -> Located a
removeMeta (A ann x) = A (region ann) x


--Remove the extra canonical annotation information, leaving just the region
commentRemoveMeta :: CommentedWithMeta a -> Commented a
commentRemoveMeta (A (ann,comment) x) = A (region ann, comment) x
