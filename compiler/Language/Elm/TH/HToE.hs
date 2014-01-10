-----------------------------------------------------------------------------
--
-- Module      :  Language.Elm.TH.HToE
-- Copyright   :  Copyright: (c) 2011-2013 Joey Eremondi
-- License     :  BSD3
--
-- Maintainer  :  joey.eremondi@usask.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- |
--
-----------------------------------------------------------------------------
module Language.Elm.TH.HToE where

{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

import Language.Haskell.TH.Syntax

import Data.Aeson.TH


import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T

import Data.List (isPrefixOf)

import Language.Haskell.TH.Desugar.Sweeten
import Language.Haskell.TH.Desugar

import Language.Elm.TH.Util
--import Parse.Expression (makeFunction)

import Control.Applicative


{-|
Haskell to Elm Translations
Most of these functions operate in the Q monad, so that we can
compare against Haskell expressions or types in quotes (see isIntType etc.)

The return value is a list of Elm declarations
-}




-- |Translate a constructor into a list of Strings and type-lists,
-- Which Elm uses for its internal representation of constructors
translateCtor :: Con -> Q (String,[T.Type])
translateCtor (NormalC name strictTyList) =  do
  let sndList = map snd strictTyList
  tyList <- mapM translateType sndList
  return (nameToString name, tyList)
--TODO handle record decs



--------------------------------------------------------------------------
-- | Helper to get the fields of the Clause type
unClause :: Clause -> ([Pat], Body, [Dec])
unClause (Clause p b d) = (p, b, d)

-- |Helper for putting elements in a list
single :: a -> [a]
single a = [a]

{-|Translate a Haskell declaration into an Elm Declaration
  Currently implemented:
    ADTs
    Functions
    Value declarations
    Type synonyms

-}

translateDec:: Dec -> Q [D.Declaration]

--TODO translate where decs into elm let-decs
--TODO what about when more than one clause?
translateDec (FunD name [Clause patList body _where])  = do
    let eName = nameToString name
    eBody <- translateBody body
    ePats <- mapM translatePattern patList
    return $ single $ D.Definition $ E.Definition (P.PVar eName) (makeFunction ePats (Lo.none eBody)) Nothing --TODO what is maybe arg?

translateDec (ValD pat body _where)  = do
    eBody <- translateBody body
    ePat <- translatePattern pat
    return $ single $ D.Definition $ E.Definition ePat (Lo.none eBody) Nothing --TODO what is maybe arg?


translateDec dec@(DataD [] name tyBindings ctors names) = do
    --jsonDecs <- deriveFromJSON defaultOptions name
    eCtors <- mapM translateCtor ctors
    return [ D.Datatype eName eTyVars eCtors []] --TODO derivations?
    where
        eName = nameToString name
        eTyVars = map (nameToString . tyVarToName) tyBindings


--TODO data case for non-empty context?
translateDec (DataD cxt name tyBindings ctors names) = unImplemented "Data decl with context"

translateDec (NewtypeD cxt name tyBindings  ctor nameList) = unImplemented "Newtypes"

translateDec (TySynD name tyBindings ty) = do
    let eName = nameToString name
    let eTyVars = map (nameToString . tyVarToName) tyBindings
    eTy <- translateType ty
    return $ single $ D.TypeAlias eName eTyVars eTy []

translateDec (ClassD cxt name tyBindings funDeps decs ) = unImplemented "Class definitions"
translateDec (InstanceD cxt ty decs) = unImplemented "Instance declarations"

--TODO fix signatures
translateDec (SigD name ty) = return []--(single . D.Definition . (E.TypeAnnotation (nameToString name)) ) <$> translateType ty
translateDec (ForeignD frn) = unImplemented "FFI declarations"


translateDec (PragmaD pragma)  = unImplemented "Haskell Pragmas"


translateDec (FamilyD famFlavour name [tyVarBndr] mKind) = unImplemented "Type families"

translateDec (DataInstD cxt name types ctors names) = unImplemented "Data instances"


translateDec (NewtypeInstD cxt name types ctor names) = unImplemented "Newtypes instances"

translateDec (TySynInstD name types theTy) = unImplemented "Type synonym instances"

--------------------------------------------------------------------------
-- | Convert a declaration to an elm Definition
-- Only works on certain types of declarations TODO document which

translateDef :: Dec -> Q E.Def

--TODO non-empty where?
translateDef (ValD pat body _where) = do
    ePat <- translatePattern pat
    eExp <- translateBody body
    return $ E.Definition ePat (Lo.none eExp) Nothing

translateDef d = unImplemented "Non-simple function/value definitions"

--------------------------------------------------------------------------
-- |Translate a pattern match from Haskell to Elm

translatePattern :: Pat -> Q P.Pattern

translatePattern (LitP lit) = P.PLiteral <$> translateLiteral lit

translatePattern (VarP name) = return $ P.PVar $ nameToString name

translatePattern (TupP patList) = P.tuple <$> mapM translatePattern patList


translatePattern (ConP name patList) = (P.PData $ nameToString name) <$> mapM translatePattern patList

--Just pass through parentheses
translatePattern (ParensP p) = translatePattern p

--TODO Infix, tilde, bang, as, record,  view


translatePattern WildP = return P.PAnything

--Ignore the type signature if there's one in the pattern
translatePattern (SigP pat _) = translatePattern pat

translatePattern (ListP patList) = P.list <$> mapM translatePattern patList

translatePattern _ = unImplemented "Misc patterns"

--------------------------------------------------------------------------
-- |Translate a function body into Elm
-- Note that guarded bodies are currently unsupported
translateBody  :: Body -> Q E.Expr
translateBody (NormalB e) = translateExpression e

-- | Expression helper function to convert a Var to a String
expressionToString (VarE name) = nameToString name

-- | Generic elm expression for "otherwise"
elmOtherwise = E.Var "otherwise"

-- | Translate a guard into an Elm expression
translateGuard (NormalG exp) = translateExpression exp
translateGuard _ = unImplemented "Pattern-match guards"
--------------------------------------------------------------------------
{-|Translate a haskell Expression into Elm
Currently supported:
  Variables
  Literals
  Lambdas
  Constructors
  Function Application
  Parenthises
  tuples
  Conditionals
  Multi-way If statements
  Let-expressions
  Case expressions
  List literals
  Infix operations
  
Supported but not translated:
  Type signatures
-}
translateExpression :: Exp -> Q E.Expr

--TODO multi pattern exp?
translateExpression (LamE [pat] expBody) = do
  ePat <- translatePattern pat
  eBody <- translateExpression expBody
  return $ E.Lambda ePat (Lo.none eBody)

translateExpression (VarE name) =  return $ E.Var $ nameToString name

--Just treat constructor as variable --TODO is this okay?
translateExpression (ConE name) = return $ E.Var $ nameToString name

translateExpression (LitE lit) = E.Literal <$> translateLiteral lit

--Lo.none converts expressions to located expressions with no location

translateExpression (AppE fun arg) = do
    eFun <- translateExpression fun
    eArg <- translateExpression arg
    return $ E.App (Lo.none eFun) (Lo.none eArg)

--TODO infix stuff, ranges, record con, record update

translateExpression (ParensE e) = translateExpression e

translateExpression (TupE es) = (E.tuple . map Lo.none) <$> mapM translateExpression es

translateExpression (CondE cond th el) = do
    eCond <- Lo.none <$> translateExpression cond
    eTh <- Lo.none <$> translateExpression th
    eEl <- Lo.none <$> translateExpression el
    let loOtherwise = Lo.none elmOtherwise
    return $ E.MultiIf [(eCond, eTh), (loOtherwise, eEl)]

translateExpression (MultiIfE guardExpList) = do
    expPairs <- mapM transPair guardExpList
    return $ E.MultiIf expPairs
    where
        transPair (guard, exp) = do
            eGuard <- translateGuard guard
            eExp <- translateExpression exp
            return (Lo.none eGuard, Lo.none eExp)

translateExpression (LetE decList exp) = do
    eDecs <- mapM translateDef decList
    eExp <- translateExpression exp
    return $ E.Let eDecs (Lo.none eExp)

--TODO deal with Where
translateExpression (CaseE exp matchList) = do
    eExp <- translateExpression exp
    eMatch <- mapM getMatch matchList
    return $ E.Case (Lo.none eExp) eMatch
    where
      getMatch (Match pat body _decList) = do
        ePat <- translatePattern pat
        eBody <- translateBody body
        return (ePat, Lo.none eBody)

translateExpression (ListE exps) = (E.ExplicitList . map Lo.none) <$> mapM translateExpression exps

--Infix where we have all the parts
translateExpression (InfixE (Just e1) op (Just e2)) = do
    eE1 <- translateExpression e1
    eE2 <- translateExpression e2
    let eOp =  expressionToString op
    return $ E.Binop eOp (Lo.none eE1) (Lo.none eE2)

--Just ignore signature
translateExpression (SigE exp _) = translateExpression exp

--Just ignore signature
translateExpression e = unImplemented $ "Misc expression " ++ show e

--------------------------------------------------------------------------
-- |Translate a literal value from Haskell to Elm
-- Strings are translated into strings, not char lists

translateLiteral :: Lit-> Q  L.Literal
translateLiteral = return . noQTrans where
    noQTrans (CharL c) = L.Chr c

    noQTrans (StringL s) = L.Str s

    noQTrans (IntegerL i) = L.IntNum $ fromInteger i

    noQTrans (IntPrimL i) =  L.IntNum $ fromInteger i

    noQTrans (FloatPrimL f) = L.FloatNum $ fromRational f

    noQTrans (DoublePrimL f) = L.FloatNum $ fromRational f

    noQTrans (RationalL f) = L.FloatNum $ fromRational f

    noQTrans _ = unImplemented "Misc literals"






--------------------------------------------------------------------------
{-|
Translate a Haskell type into an Elm type
Currently translates primitive types, lists, tuples and constructors (ADTs)
Doesn't support type classes or fancier types
-}
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
          (TupleT 0) -> return T.EmptyRecord
          --Lists and tuples, just Data in Elm
          (AppT ListT t) -> do
            et <- translateType t
            return $ T.listOf et
          _ -> unImplemented "misc types"

          
