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

import Parse.Expression (makeFunction)

import Control.Applicative

--Main entry point, and the only public function
--TODO make others private
toElm :: [Dec] -> Q [D.Declaration () ()]
toElm decs = do
  retList <- mapM translateDec decs
  return retList

--TODO remove
unImplemented = error "Not implemented"

{-|
Haskell to Elm Translations
Most of these functions operate in the Q monad, so that we can
compare against Haskell expressions or types in quotes (see isIntType etc.)

The return value is a list of Elm declarations
TODO return a module?
-}

-- |Translate a type variable to a name, ignoring its kind
tyVarToName :: TyVarBndr -> Name
tyVarToName (PlainTV n) = n
tyVarToName (KindedTV n _ ) = n

--Abstract out the translation of names to strings
--So that we can modify if need be
--Right now is just a synonym
nameToString :: Name -> String
nameToString = nameBase--TODO fancier?

-- |Translate a constructor into a list of Strings and type-lists,
-- Which Elm uses for its internal representation of constructors
translateCtor :: Con -> Q (String,[T.Type])
translateCtor (NormalC name strictTyList) =  do
  let sndList = map snd strictTyList
  tyList <- mapM translateType sndList
  return (nameToString name, tyList)
--TODO handle record decs



--------------------------------------------------------------------------
-- | Helper to translate clauses
unClause :: Clause -> ([Pat], Body, [Dec])
unClause (Clause p b d) = (p, b, d)

{-|Translate a Haskell declaration into an Elm Declaration
  Currently implemented:
    ADTs

  TODO make a special error for non-supported features
-}

translateDec:: Dec -> Q (D.Declaration () () )

--TODO translate where decs into elm let-decs
--TODO what about when more than one clause?
translateDec (FunD name [Clause patList body _where])  = do
    let eName = nameToString name
    eBody <- translateBody body
    ePats <- mapM translatePattern patList
    return $ D.Definition $ E.Def (P.PVar eName) (makeFunction ePats (Lo.none eBody))

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
-- | Convert a declaration to an elm Definition
-- Only works on certain types of declarations TODO document which

translateDef :: Dec -> Q (E.Def t v)

--TODO non-empty where?
translateDef (ValD pat body _where) = do
    ePat <- translatePattern pat
    eExp <- translateBody body
    return $ E.Def ePat (Lo.none eExp)

translateDef d = unImplemented

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

translatePattern _ = unImplemented

--------------------------------------------------------------------------
-- |Translate a function body into Elm
-- Note that guarded bodies are currently unsupported
translateBody  :: Body -> Q (E.Expr t v)
translateBody (NormalB e) = translateExpression e


--------------------------------------------------------------------------
{-|Translate a haskell Expression into Elm
Currently supported: Variables, literals
-}
translateExpression :: Exp -> Q (E.Expr t v)

translateExpression (VarE name) = return $ E.Var $ nameToString name

translateExpression (ConE name) = unImplemented --TODO how to do this one?

translateExpression (LitE lit) = E.Literal <$> translateLiteral lit

--Lo.none converts expressions to located expressions with no location

translateExpression (AppE fun arg) = do
    eFun <- translateExpression fun
    eArg <- translateExpression arg
    return $ E.App (Lo.none eFun) (Lo.none eArg)

--TODO infix stuff, ranges, record con, record update

translateExpression (ParensE e) = translateExpression e

translateExpression (TupE es) = (E.tuple . (map Lo.none)) <$> mapM translateExpression es

-- TODO if?
translateExpression (CondE cond th el) = unImplemented

translateExpression (MultiIfE guardExpList) = unImplemented

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

translateExpression (ListE exps) = (E.ExplicitList . (map Lo.none)) <$> mapM translateExpression exps

--Just ignore signature
translateExpression (SigE exp _) = translateExpression exp

--------------------------------------------------------------------------
-- |Translate a literal value from Haskell to Elm
-- Strings are translated into strings, not char lists

translateLiteral :: Lit-> Q  L.Literal
translateLiteral = (return . noQTrans) where
    noQTrans (CharL c) = L.Chr c

    noQTrans (StringL s) = L.Str s

    noQTrans (IntegerL i) = L.IntNum $ fromInteger i

    noQTrans (IntPrimL i) =  L.IntNum $ fromInteger i

    noQTrans (FloatPrimL f) = L.FloatNum $ fromRational f

    noQTrans (DoublePrimL f) = L.FloatNum $ fromRational f

    noQTrans (RationalL f) = L.FloatNum $ fromRational f

    noQTrans _ = unImplemented




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
          (TupleT 0) -> return $ T.EmptyRecord
          --Lists and tuples, just Data in Elm
          (AppT ListT t) -> do
            et <- translateType t
            return $ T.listOf (et)
          _ -> unImplemented

