{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
    ( version, rawVersion
    , parseDependencies
    , compile, Context(..), Result(..)
    , Object(..)
    , Dealiaser, dummyDealiaser
    , Error, errorToString, errorToJson, printError
    , Warning, warningToString, warningToJson, printWarning
    , getUsedDefs, cleanObject, renderObject
    ) where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Binary as Binary
import qualified Data.List as List

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

import Control.Applicative ((<$>) )
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
      do  modul <- Compile.compile user packageName isRoot unwrappedInterfaces source
          dceInfo<- DCE.analyzeModule modul
          docs <- docsGen isExposed modul

          let interface = Module.toInterface modul
          let optModule = Optimize.optimize modul
          let (topHeader, fnHeader, jsExports, fnDefs, modulName ) = JS.generate optModule

          return (Result docs interface $
                  Object
                    { _topHeader = topHeader
                    , _fnHeader = fnHeader
                    , _jsExports = jsExports
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


-- JavaScript as it is stored on disk
-- This format allows us to remove unused functions from imported modules,
-- as well as helping with inlining 
data Object = Object
    { _topHeader :: Text.Text
    , _fnHeader :: Text.Text
    , _jsExports :: [(String, Text.Text )]
    , _fnDefs :: [(String, Text.Text )]
    , _fnRefGraph :: [(([String], String), [([String], String)])]
    , _objModule :: PublicModule.Name 
    }


-- Store Objects on disk, encoding Text as UTF8
instance Binary.Binary Object where
  put o =
    do  let (PublicModule.Name names) = _objModule o
        Binary.put $ map (encodeUtf8 . Text.pack ) names
        Binary.put $ encodeUtf8 $ _topHeader o
        Binary.put $ encodeUtf8 $ _fnHeader o
        Binary.put $ map (\(s, exprt ) ->
                           (encodeUtf8 $ Text.pack s, encodeUtf8 exprt ) ) $ _jsExports o
        Binary.put $ map (\(s, def ) ->
                           (encodeUtf8 $ Text.pack s, encodeUtf8 def ) ) $ _fnDefs o
        Binary.put $ _fnRefGraph o
        
  get =
    do  modulList <- Binary.get
        let modul =
              PublicModule.Name $ map ( Text.unpack . decodeUtf8) modulList
        topH <- decodeUtf8 <$> Binary.get
        fnH <- decodeUtf8 <$> Binary.get
        expPairList <- Binary.get
        let unpackedExports =
              map (\(tnm, texp ) -> (Text.unpack $ decodeUtf8 tnm, decodeUtf8 texp) ) expPairList
        defPairList <- Binary.get
        let unpackedDefs =
              map (\(tnm, tdef ) -> (Text.unpack $ decodeUtf8 tnm, decodeUtf8 tdef) ) defPairList
        graphPairs <- Binary.get
        return $
          Object topH fnH unpackedExports unpackedDefs graphPairs modul


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


-- Given a list of used module names and their reference graphs
-- and the target module being compiled,
-- return the list of all names in the given modules
-- which the target module depends on
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
        , (["Signal"], Var.Value "output" )
        , (["Signal"], Var.Value "map" )
        , (["Signal"], Var.Value "map2" )
        , (["Signal"], Var.Value "filter" )
        , (["Maybe"], Var.Value "Nothing" )
        , (["Maybe"], Var.Value "Just" )
        , (["Result"], Var.Value "Err" )
        , (["Result"], Var.Value "Ok" )
        , (["Char"], Var.Value "isDigit" )
        , (["Array"], Var.Value "toList" )
        , (["Dict"], Var.Value "toList" )
        , (["Dict"], Var.Value "empty" )
        , (["Dict"], Var.Value "update" )
        , (["List"], Var.Value "map" )
        , (["List"], Var.Value "length" )
        , (["Transform"], Var.Value "matrix" )
        , (["Transform"], Var.Value "multiply" )
        , (["Transform"], Var.Value "rotation" )
        ]

    reachableImports =
      DCE.reachableImports nameGraphs stringValues
  in
    Set.fromList reachableImports


-- Given a set of module-qualified names
-- Remove any top-level definitions from the module
-- which are not in the set 
cleanObject
  :: Set.Set ([String], String)
  -> Object
  -> Object
cleanObject usedVars obj =
  let
    (PublicModule.Name ourName ) =
      _objModule obj

    isUsed (defName, _) =
      Set.member (ourName, defName) usedVars
  in
    obj { _fnDefs = filter isUsed $ _fnDefs obj
        , _jsExports = filter isUsed $ _jsExports obj}


renderObject :: Object -> Text.Text
renderObject obj =
  let
    PublicModule.Name nameList = _objModule obj
    makeName =
      List.intercalate "." (["Elm"] ++ nameList ++ ["make"] )
    valuesName =
      Text.pack $
        List.intercalate "." (["_elm"] ++ nameList ++ ["values"] )
    opAssign =
      Text.pack "_op : _op"
    valuesList =
      Text.concat
      [ Text.pack "{ "
      , Text.intercalate (Text.pack ",\n") (opAssign : (map snd $ _jsExports obj) )
      , Text.pack "};"
      ]
  in
    Text.concat
    [ _topHeader obj
    , Text.pack ("\n" ++ makeName ++ " = function(_elm){\n")
    , _fnHeader obj
    , Text.intercalate (Text.pack "\n") $ map snd $ _fnDefs obj
    , Text.concat
      [ valuesName
      , Text.pack " = "
      , valuesList
      , Text.pack $ "\nreturn "
      ,valuesName
      , Text.pack ";"
      ]
    , Text.pack "};"
    ]
