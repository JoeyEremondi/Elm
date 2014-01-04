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
nameToString n = show n --TODO fancier?

translateCtor :: Con -> (String,[T.Type])
translateCtor (NormalC name strictTyList) =  (nameToString name, map (translateType . snd) strictTyList)
--TODO handle record decs

toElm :: [Dec] -> [D.Declaration () ()]
toElm decs = map translateDec decs

--------------------------------------------------------------------------

translateDec:: Dec -> D.Declaration () ()

translateDec (FunD name clauseList) = unImplemented
translateDec (ValD pat body decs) = unImplemented

translateDec (DataD [] name tyBindings ctors names) =
    D.Datatype eName eTyVars eCtors
    where
        eName = nameToString name
        eTyVars = map (nameToString . tyVarToName) tyBindings
        eCtors = map translateCtor ctors

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

isTupleType (AppT (TupleT _arity) _) = True
isTupleType (AppT t1 t2) = isTupleType t1
isTupleType _ = False

tupleTypeToList (AppT (TupleT _arity) t) = [t]
tupleTypeToList (AppT t1 t2) = (tupleTypeToList t1) ++ [t2]

--------------------------------------------------------------------------

translateType :: Type -> T.Type
--type variables
translateType (VarT name) = T.Var (nameToString name)
--sum types/ADTs
translateType (ConT name) = T.Data (nameToString name) [] --TODO what is this list param?
--functions
translateType (AppT (AppT ArrowT a) b) = T.Lambda ea eb
    where ea = translateType a
          eb = translateType b
--empty tuple/record
translateType (TupleT 0) = T.EmptyRecord
--Lists and tuples, just Data in Elm
translateType (AppT ListT t) = T.listOf (et)
    where et = translateType t
--TODO fill in other cases, esp records
--Cases which aren't captured by basic pattern matching
translateType t
    | isTupleType t = T.tupleOf (map translateType $ tupleTypeToList t)
    | otherwise = unImplemented
