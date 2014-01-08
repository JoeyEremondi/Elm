module Elm.Haskelm.BuildString (build, buildAll) where

import Control.Monad (when)
import qualified Data.Binary as Binary
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Language.Haskell.TH (Q, runIO)

import qualified Transform.Canonicalize as Canonical

import qualified Data.ByteString.Lazy as L

import qualified Build.Utils as Utils
import qualified Build.Flags as Flag
import qualified Build.Source as Source
import qualified Build.Print as Print
import qualified Generate.JavaScript as JS
import qualified InterfaceSerialization as IS
import qualified Parse.Module as Parser
import qualified SourceSyntax.Module as M



import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified System.Console.CmdArgs as CmdArgs
import System.Directory
import System.FilePath
import GHC.Conc

import Elm.Haskelm.Dependencies (getSortedDependencies)
import qualified Generate.Html as Html
import qualified Metadata.Prelude as Prelude
import qualified Build.Utils as Utils
import qualified Build.Flags as Flag
import qualified Build.File as File
import qualified Elm.Internal.Paths as Path

import Build.Flags (flags)


build :: Flag.Flags -> Int -> M.Interfaces -> String -> [FilePath]
  -> Q (String, M.Interfaces)
build _ _ interfaces moduleName [] =
    return (moduleName, interfaces)
build flags numModules interfaces _ (filePath:rest) =
    do (name,interface) <-
           build1 flags (numModules - length rest) numModules interfaces filePath
       let interfaces' = Map.insert name interface interfaces
       build flags numModules interfaces' name rest


build1 :: Flag.Flags -> Int -> Int -> M.Interfaces -> FilePath
  -> Q (String, M.ModuleInterface)
build1 flags moduleNum numModules interfaces filePath =
    do compiled <- alreadyCompiled flags filePath
       case compiled of
         False -> compile flags number interfaces filePath
         --True  -> retrieve flags interfaces filePath
    where
      number = "[" ++ show moduleNum ++ " of " ++ show numModules ++ "]"

alreadyCompiled _ _ = return False --TODO fix      
{-
alreadyCompiled :: Flag.Flags -> FilePath -> Q Bool
alreadyCompiled flags filePath = do
  existsi <- doesFileExist (Utils.elmi flags filePath)
  existso <- doesFileExist (Utils.elmo flags filePath)
  if not existsi || not existso
    then return False
    else do tsrc <- getModificationTime filePath
            tint <- getModificationTime (Utils.elmo flags filePath)
            return (tsrc <= tint)
-}
            
{-            
retrieve :: Flag.Flags -> Map.Map String M.ModuleInterface -> FilePath
         -> Q (String, M.ModuleInterface)
retrieve flags interfaces filePath = do
  bytes <- IS.loadInterface (Utils.elmi flags filePath)
  let binary = IS.interfaceDecode (Utils.elmi flags filePath) =<< bytes
  case IS.validVersion filePath =<< binary of
    Right (name, interface) ->
        do when (Flag.print_types flags) (Print.interfaceTypes interfaces interface)
           return (name, interface)
    Left err ->
        do hPutStrLn stderr err
           exitFailure
-}

compile :: Flag.Flags -> String -> M.Interfaces -> FilePath
        -> Q (String, M.ModuleInterface)
compile flags number interfaces source = do
  --do source <- readFile filePath
     let name = getName source
     --printStatus name

     --createDirectoryIfMissing True (Flag.cache_dir flags)
     --createDirectoryIfMissing True (Flag.build_dir flags)

     metaModule <-
         case Source.build (Flag.no_prelude flags) interfaces source of
           Right modul -> return modul
           Left errors -> runIO $ do
             writeFile "errors.elm" source
             Print.errors errors
             exitFailure

     --when (Flag.print_types flags) $ Print.metaTypes interfaces metaModule

     let intermediate = (name, Canonical.interface name $ M.metaToInterface metaModule)
     --generateCache intermediate metaModule
     return intermediate

  where
    filePath = "filePath TODO remove"
    getName source = case Parser.getModuleName source of
                       Just n -> n
                       Nothing -> "Main"

    printStatus name =
        hPutStrLn stdout $ concat [ number, " Compiling ", name
                                  , replicate (max 1 (20 - length name)) ' '
                                  , "( " ++ filePath ++ " )" ]

    generateCache intermediate metaModule = do
      createDirectoryIfMissing True . dropFileName $ Utils.elmi flags filePath
      writeFile (Utils.elmo flags filePath) (JS.generate metaModule)
      withBinaryFile (Utils.elmi flags filePath) WriteMode $ \handle ->
          L.hPut handle (Binary.encode intermediate)
          
buildAll :: Flag.Flags -> [(String, String)]  -> Q (String)
buildAll flags moduleList = do
       let modules = Map.fromList moduleList
       let noPrelude = Flag.no_prelude flags
       --builtIns <- if noPrelude then return Map.empty else Prelude.interfaces
       let builtIns = Map.empty --TODO need this?
       files <- getSortedDependencies builtIns modules

       (moduleName, interfaces) <-
           build flags (length files) builtIns "" files

       --js <- foldM appendToOutput BS.empty files
       let js = BS.empty --TODO fix
       
       (extension, code) <-
           if Flag.only_js flags
           then do --putStr "Generating JavaScript ... "
                   return ("js", js)
           else do --putStr "Generating HTML ... "
                   return (makeHtml js moduleName)
       return $ show code
       --let targetFile = Utils.buildPath flags rootFile extension
       --createDirectoryIfMissing True (takeDirectory targetFile)
       --BS.writeFile targetFile code
       --putStrLn "Done"

    where
      appendToOutput :: BS.ByteString -> FilePath -> IO BS.ByteString
      appendToOutput js filePath = do
        src <- BS.readFile (Utils.elmo flags filePath)
        return (BS.append src js)

      sources js = map Html.Link (Flag.scripts flags) ++ [ Html.Source js ]

      makeHtml js moduleName = ("html", BS.pack $ renderHtml html)
          where
            rtsPath = Maybe.fromMaybe Path.runtime (Flag.runtime flags)
            html = Html.generate rtsPath (takeBaseName "rootFile_TODO_fix") (sources js) moduleName ""
