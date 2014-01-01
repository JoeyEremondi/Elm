module Elm.Haskelm.EToH (translate) where

import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Variable as V

import qualified Language.Haskell.TH.Syntax as TH

notImplemented = error "Not Implemented"


translate :: M.Module Int Int -> [TH.Dec]--TODO fix
translate m = notImplemented

translateDecl :: D.Declaration Int Int -> TH.Dec
translateDecl m = notImplemented