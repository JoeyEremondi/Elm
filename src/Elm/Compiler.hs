{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
    ( version, rawVersion
    , parseDependencies
    , compile, Context(..), Result(..)
    , Object(..)
    , Dealiaser, dummyDealiaser
    , Error, errorToString, errorToJson, printError
    , Warning, warningToString, warningToJson, printWarning
    , getUsedDefs, cleanObject
    ) where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS

import Data.Text.Internal ( )

import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified Compile
import qualified Docs.Check as Docs
import qualified Elm.Compiler.Module as PublicModule
import qualified Elm.Compiler.Version as Version
import qualified Elm.Docs as Docs
import qualified Generate.JavaScript as JS
import qualified Optimize
import qualified Optimize.DeadCode as DCE
import qualified Parse.Module as Parse
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning

import Control.Applicative ((<$>), (<*>) )
import Data.Text.Encoding (encodeUtf8, decodeUtf8)



-- VERSION

version :: String
version =
    Version.version


rawVersion :: [Int]
rawVersion =
    Version.rawVersion


-- DEPENDENCIES

parseDependencies
    :: String
    -> Either [Error] (PublicModule.Name, [PublicModule.Name])
parseDependencies sourceCode =
  let
    (Result.Result _warnings rawResult) =
      Parse.parse sourceCode Parse.header
  in
    case rawResult of
      Result.Err msgs ->
          Left $ map (Error . A.map Error.Syntax) msgs

      Result.Ok (Module.Header names _docs _exports imports) ->
          Right
            ( PublicModule.Name names
            , map (PublicModule.Name . fst . A.drop) imports
            )


-- COMPILATION

{-| Compiles Elm source code to JavaScript. -}
compile
    :: Context
    -> String
    -> Map.Map PublicModule.Name PublicModule.Interface
    -> (Dealiaser, [Warning], Either [Error] Result)

compile context source interfaces =
  let
    (Context user packageName isRoot isExposed) =
      context

    unwrappedInterfaces =
      Map.mapKeysMonotonic (\(PublicModule.Name name) -> name) interfaces

    (Result.Result (dealiaser, warnings) rawResult) =
      do  initialModul <- Compile.compile user packageName isRoot unwrappedInterfaces source
          (dceModul , dceInfo) <- DCE.analyzeModule initialModul
          docs <- docsGen isExposed dceModul

          let interface = Module.toInterface dceModul
          let optModule = Optimize.optimize dceModul
          let (topHeader, fnHeader, fnDefs, modulName ) = JS.generate optModule

          return (Result docs interface $
                  Object
                    { _topHeader = topHeader
                    , _fnHeader = fnHeader
                    , _fnDefs = fnDefs
                    , _fnRefGraph = dceInfo
                    , _objModule = PublicModule.Name modulName})
  in 
    ( maybe dummyDealiaser Dealiaser dealiaser
    , map Warning warnings
    , Result.destruct (Left . map Error) Right rawResult
    ) 


data Context = Context
    { _user :: String
    , _packageName :: String
    , _isRoot :: Bool
    , _isExposed :: Bool
    }


data Result = Result
    { _docs :: Maybe Docs.Documentation
    , _interface :: PublicModule.Interface
    , _js :: Object
    }

data Object = Object
    { _topHeader :: Text.Text
    , _fnHeader :: Text.Text
    --, _fnFooter :: Text.Text
    , _fnDefs :: [(String, Text.Text )]
    , _fnRefGraph :: [(([String], String), [([String], String)])]
    , _objModule :: PublicModule.Name 
    } deriving (Show)


instance Binary.Binary Object where
  put o =
    do  let (PublicModule.Name names) = _objModule o
        Binary.put $ map (encodeUtf8 . Text.pack ) names
        Binary.put $ encodeUtf8 $ _topHeader o
        Binary.put $ encodeUtf8 $ _fnHeader o
        Binary.put $ map (\(s, def ) ->
                           (encodeUtf8 $ Text.pack s, encodeUtf8 def ) ) $ _fnDefs o
        Binary.put $ _fnRefGraph o
        
  get =
    do  modulList <- Binary.get
        let modul =
              PublicModule.Name $ map ( Text.unpack . decodeUtf8) modulList
        topH <- decodeUtf8 <$> Binary.get
        fnH <- decodeUtf8 <$> Binary.get
        defPairList <- Binary.get
        let unpackedDefs =
              map (\(tnm, tdef ) -> (Text.unpack $ decodeUtf8 tnm, decodeUtf8 tdef) ) defPairList
        graphPairs <- Binary.get
        return $
          Object topH fnH unpackedDefs graphPairs modul


docsGen
    :: Bool
    -> Module.CanonicalModule
    -> Result.Result w Error.Error (Maybe Docs.Documentation)
docsGen isExposed modul =
  if not isExposed then
    Result.ok Nothing
  else
    let
      getChecked =
        Docs.check (Module.exports modul) (Module.docs modul)

      toDocs checked =
        Docs.fromCheckedDocs (PublicModule.Name (Module.names modul)) checked
    in
      (Just . toDocs) `fmap` Result.mapError Error.Docs getChecked


-- DEALIASER

newtype Dealiaser =
    Dealiaser P.Dealiaser


dummyDealiaser :: Dealiaser
dummyDealiaser =
    Dealiaser Map.empty


-- ERRORS

newtype Error =
    Error (A.Located Error.Error)


errorToString :: Dealiaser -> String -> String -> Error -> String
errorToString (Dealiaser dealiaser) location source (Error err) =
    Error.toString dealiaser location source err


printError :: Dealiaser -> String -> String -> Error -> IO ()
printError (Dealiaser dealiaser) location source (Error err) =
    Error.print dealiaser location source err


errorToJson :: Dealiaser -> String -> Error -> Json.Value
errorToJson (Dealiaser dealiaser) location (Error err) =
    Error.toJson dealiaser location err


-- WARNINGS

newtype Warning =
    Warning (A.Located Warning.Warning)


warningToString :: Dealiaser -> String -> String -> Warning -> String
warningToString (Dealiaser dealiaser) location source (Warning err) =
    Warning.toString dealiaser location source err


printWarning :: Dealiaser -> String -> String -> Warning -> IO ()
printWarning (Dealiaser dealiaser) location source (Warning err) =
    Warning.print dealiaser location source err


warningToJson :: Dealiaser -> String -> Warning -> Json.Value
warningToJson (Dealiaser dealiaser) location (Warning err) =
    Warning.toJson dealiaser location err


getUsedDefs
  :: [(PublicModule.Name, [(([String], String), [([String], String)])])]
  -> [(PublicModule.Name, PublicModule.Interface)]
  -> Set.Set ([String], String)
getUsedDefs refGraphs startIfaces =
  let
    exports =
      [(nm, exprt) | (PublicModule.Name nm, iface ) <- startIfaces, exprt <- Module.iExports iface]
        ++ alwaysUsed
        ++ [(nm, Var.Value "main") | (PublicModule.Name nm, _ ) <- startIfaces]

    unValue (nm, v) =
      case v of
        Var.Value s -> Just (nm, s)
        _ -> Nothing

    stringValues =
      Maybe.catMaybes $ map unValue exports

    nameGraphs =
      map (\(PublicModule.Name nm, x) -> (nm, x) ) refGraphs

    alwaysUsed =
        [ (["Signal"], Var.Value "constant" )
        , (["Maybe"], Var.Value "Nothing" )
        , (["Maybe"], Var.Value "Just" )
        ]
    reachableImports =
      DCE.reachableImports nameGraphs stringValues
  in
    Set.fromList reachableImports

cleanObject
  :: Set.Set Var.Canonical
  -> Object
  -> Object
cleanObject usedVars obj =
  let
    (PublicModule.Name ourName ) = _objModule obj
    isUsed (defName, _) = Set.member (Var.Canonical (Var.Module ourName) defName) usedVars
  in
    obj {_fnDefs = filter isUsed $ _fnDefs obj}
