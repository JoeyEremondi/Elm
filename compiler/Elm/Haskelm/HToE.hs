-----------------------------------------------------------------------------
--
-- Module      :  Elm.Haskelm.HToE
-- Copyright   :  Copyright: (c) 2011-2013 Joey Eremondi
-- License     :  BSD3
--
-- Maintainer  :  info@elm-lang.org
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Elm.Haskelm.HToE where

import Language.Haskell.TH.Syntax


import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Variable as V

unImplemented = error "Not implemented"

--Translate a type variable to a name, ignoring its kind
tyVarToName :: TyVarBndr -> Name
tyVarToName (PlainTV n) = n
tyVarToName (KindedTV n _ ) = n

nameToString :: Name -> String
nameToString n = nameBase n --TODO fancier?

translateCtor :: Con -> Q (String,[T.Type])
translateCtor (NormalC name strictTyList) =  do
  let sndList = map snd strictTyList
  tyList <- mapM translateType sndList
  return (nameToString name, tyList)
--TODO handle record decs

toElm :: [Dec] -> Q [D.Declaration () ()]
toElm decs = do
  retList <- mapM translateDec decs
  return retList

--------------------------------------------------------------------------

translateDec:: Dec -> Q (D.Declaration () () )

translateDec (FunD name clauseList) = unImplemented
translateDec (ValD pat body decs) = unImplemented

translateDec (DataD [] name tyBindings ctors names) = do
    eCtors <- mapM translateCtor ctors
    return $ D.Datatype eName eTyVars eCtors
    where
        eName = nameToString name
        eTyVars = map (nameToString . tyVarToName) tyBindings
        

--TODO data case for non-empty context?
translateDec (DataD cxt name tyBindings ctors names) = unImplemented

translateDec (NewtypeD cxt name tyBindings  ctor nameList) = unImplemented

translateDec (TySynD name tyBindings ty) = unImplemented
translateDec (ClassD cxt name tyBindings funDeps decs ) = unImplemented
translateDec (InstanceD cxt ty decs) = unImplemented

translateDec (SigD name ty) = unImplemented
translateDec (ForeignD frn) = unImplemented


translateDec (PragmaD pragma)  = unImplemented


translateDec (FamilyD famFlavour name [tyVarBndr] mKind) = unImplemented

translateDec (DataInstD cxt name types ctors names) = unImplemented


translateDec (NewtypeInstD cxt name types ctor names) = unImplemented

translateDec (TySynInstD name types theTy) = unImplemented

--------------------------------------------------------------------------
--Type helper functions

int = [t| Int |]
string = [t| String |]
float = [t| Float |]
bool = [t| Bool |]

isIntType t = do
  tint <- int
  runIO $ putStrLn $ "Checking if int " ++ (show (t == tint)) 
  return (t == tint)
  
isStringType t = do
  tstr <- string
  return (t == tstr)
  
isFloatType t = do
  tfloat <- float
  return (t == tfloat)
  
isBoolType t = do
  tbool <- bool
  return (t == tbool)

isTupleType (AppT (TupleT _arity) _) = True
isTupleType (AppT t1 t2) = isTupleType t1
isTupleType _ = False


tupleTypeToList (AppT (TupleT _arity) t) = [t]
tupleTypeToList (AppT t1 t2) = (tupleTypeToList t1) ++ [t2]

--------------------------------------------------------------------------

translateType :: Type -> Q T.Type

    
--TODO fill in other cases, esp records
--Cases which aren't captured by basic pattern matching
translateType t = do
  --Unbox some Monad information that we need
  isInt <- isIntType t
  isString <- isStringType t
  isFloat <- isFloatType t
  isBool <- isBoolType t
  generalTranslate isInt isString isFloat isBool --TODO get these in scope
  where 
    generalTranslate :: Bool -> Bool -> Bool -> Bool -> Q T.Type
    generalTranslate isInt isString isFloat isBool
      | isInt = return $ T.Data "Int" []
      | isString = return $ T.Data "String" []
      | isFloat = return $ T.Data "Float" []
      | isBool = return $ T.Data "Bool" []
      | isTupleType t = do
          tyList <- mapM translateType (tupleTypeToList t)
          return $ T.tupleOf tyList
      | otherwise = case t of
          --type variables
          (VarT name) -> return $ T.Var (nameToString name)
          --sum types/ADTs
          (ConT name) -> return $ T.Data (nameToString name) [] --TODO what is this list param?
          --functions
          (AppT (AppT ArrowT a) b) -> do
            ea <- translateType a
            eb <- translateType b
            return $ T.Lambda ea eb
            
          --empty tuple/record
          (TupleT 0) -> return $ T.EmptyRecord
          --Lists and tuples, just Data in Elm
          (AppT ListT t) -> do
            et <- translateType t
            return $ T.listOf (et)
          _ -> unImplemented
          
