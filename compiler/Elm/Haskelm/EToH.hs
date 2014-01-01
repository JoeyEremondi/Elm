module Elm.Haskelm.EToH (translate) where

import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Variable as V

notImplemented = error "Not Implemented"


translate :: M.Module Int Int -> Int --TODO fix
translate m = notImplemented

translateDecl :: D.Declaration Int Int -> Int 
translateDecl m = notImplemented