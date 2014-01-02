module Elm.Haskelm.EToH  where

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Variable as V

import Language.Haskell.TH.Syntax (mkName, Q)
import Language.Haskell.TH.Lib

notImplemented = error "Not Implemented"

qNull :: Q [a]
qNull = return []

translate :: [D.Declaration tipe var] -> DecsQ--TODO fix
translate decs = do
  mapM translateDecl decs
  

translateDecl :: D.Declaration tipe var -> DecQ
translateDecl decl = 
  case decl of
       D.Definition def -> notImplemented
       D.Datatype name tvars ctors -> do
         let hname = mkName name
         let tVarNames = map mkName tvars
         --Recursively translate the list of types
         let hctors = map transCtor ctors
         ret <- dataD qNull hname [] hctors []
         return ret
         where
           transCtor (ctor, types) = do
              let tlist = map translateType types
              normalC (mkName ctor) tlist
       _ -> notImplemented

translateType :: T.Type -> StrictTypeQ       
translateType t = do
  let transT = translateType' t
  strictType notStrict transT       
       
translateType' :: T.Type -> TypeQ
translateType' theType = 
  case theType of
       T.Lambda tfrom tto -> do
         let hfrom = translateType' tfrom
         let hto = translateType' tto
         appT (appT arrowT hfrom) hto
       T.Var name -> varT $ mkName name
       T.Data name [] -> conT $ mkName name --TODO what about type variables?
       T.Data name tlist -> return $ notImplemented
       T.EmptyRecord -> tupleT 0
       --Haskell doesn't distinguish between records and ADTs
       T.Record nameTypeList ty ->  notImplemented
  


  
  
--test :: Q [Dec]
--test = return $ [ DataD [] (mkName "Foo") [] [NormalC (mkName "Bar") [], NormalC (mkName "Baz") []] []]