module Elm.Haskelm.Dependencies (getSortedDependencies) where

import Data.Data
import Control.Applicative
import Control.Monad.Error
import qualified Control.Monad.State as State
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.Directory
import System.Exit
import System.FilePath as FP
import System.IO
import Text.PrettyPrint (Doc)

import qualified SourceSyntax.Module as Module
import qualified SourceSyntax.Type as Type
import qualified Parse.Parse as Parse
import qualified Metadata.Prelude as Prelude
import qualified Transform.Check as Check
import qualified Transform.SortDefinitions as SD
import qualified Type.Inference as TI
import qualified Type.Constrain.Declaration as TcDecl
import qualified Transform.Canonicalize as Canonical
import qualified Elm.Internal.Paths as Path
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Dependencies as Deps

import Language.Haskell.TH (Q)

getSortedDependencies ::   Module.Interfaces -> Map.Map String String -> Q [String]
getSortedDependencies builtIns modules =
    do --extras <- extraDependencies
       result <- readDeps builtIns modules
       case result of
         Right deps -> sortDeps deps
         --Left err -> failure $ err ++ if Maybe.isJust extras then "" else msg
             where msg = "\nYou may need to create a " ++
                         Path.dependencyFile ++
                         " file if you\nare trying to use a 3rd party library."

failure msg = hPutStrLn stderr msg >> exitFailure

{-
extraDependencies :: Q (Maybe [FilePath])
extraDependencies =
    do exists <- doesFileExist Path.dependencyFile
       if not exists then return Nothing else Just <$> getPaths
    where
      getPaths = do
        raw <- BSC.readFile Path.dependencyFile
        case Json.eitherDecode raw of
          Right (Deps.Mini deps) -> mapM validate deps
          Left err ->
              failure $ "Error reading the " ++ Path.dependencyFile ++ " file:\n" ++ err

      validate (name,version) = do
        let path = Path.dependencyDirectory </> toPath name version
        exists <- doesDirectoryExist path
        if exists then return path else failure (notFound name version)

      toPath name version = N.toFilePath name </> show version

      notFound name version =
          unlines
          [ "Your " ++ Path.dependencyFile ++ " file says you depend on library"
          , show name ++ " " ++ show version ++ " but it was not found."
          , "You may need to install it with:"
          , ""
          , "    elm-get install " ++ show name ++ " " ++ show version ]
-}
type Deps = (FilePath, String, [String])

sortDeps :: [Deps] -> Q [String]
sortDeps depends =
    if null mistakes
      then return (concat sccs)
      --failure
      else error $ msg ++ unlines (map show mistakes)
  where
    sccs = map Graph.flattenSCC $ Graph.stronglyConnComp depends

    mistakes = filter (\scc -> length scc > 1) sccs
    msg = "A cyclical module dependency or was detected in:\n"

    

readDeps :: Module.Interfaces -> Map.Map String String -> Q (Either String [Deps])
readDeps  builtIns modules = do
  let ifaces = (Set.fromList . Map.keys) builtIns
  let root = case (Map.lookup "Main" modules) of  (Just s) -> s; Nothing -> error "Main module was not in map"
   
  State.evalStateT (go ifaces root) Set.empty
  where
    go :: Set.Set String -> FilePath -> State.StateT (Set.Set String) Q (Either String [Deps])
    go builtIns root = do
      --let txt = case (Map.lookup root modules) of  (Just s) -> s; Nothing -> error $ "module " ++ root ++ " was not in map"
      -- lift $ getFile srcDirs root
      let txt = root --TODO change naming
      case Parse.dependencies txt of
        Left err -> return $ Left $ msg ++ show err
            where msg = "Error resolving dependencies in " ++ root ++ ":\n"

        Right (name,deps) ->
            do seen <- State.get
               let realDeps = Set.difference (Set.fromList deps) builtIns
                   newDeps = Set.difference (Set.filter (not . isNative) realDeps) seen
               State.put (Set.insert name (Set.union newDeps seen))
               --rest <- mapM (go builtIns . toFilePath) (Set.toList newDeps)
               let rest = [] --TODO what is this?
               return $ Right ((root, name, Set.toList realDeps) : concat rest)



isNative name = List.isPrefixOf "Native." name

