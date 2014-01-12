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
  return (nameToElmString name, tyList)
--TODO handle record decs

-- | Take a list of declarations and a body
-- and put it in a let only if the declarations list is non-empty
maybeLet :: [E.Def] -> E.Expr -> E.Expr 
maybeLet eWhere eBody = 
        if null eWhere
          then  eBody
          else E.Let eWhere (Lo.none eBody)

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
translateDec (FunD name [Clause patList body whereDecs])  = do
    let eName = nameToElmString name
    (ePats, asDecList) <- unzip <$> mapM translatePattern patList
    let asDecs = concat asDecList
    eWhere <- mapM translateDef whereDecs
    let eDecs = asDecs ++ eWhere
    fnBody <- translateBody body
    let eBody = maybeLet eDecs fnBody
    return $ single $ D.Definition $ E.Definition (P.PVar eName) (makeFunction ePats (Lo.none eBody)) Nothing --TODO what is maybe arg?
    
--multi-clause case i.e. pattern matching
--Convert to a single-clause function with a case statement
translateDec (FunD name clauseList) = do
  let ((Clause patList _ _):_) = clauseList
  let numArgs = length patList
  let argStrings = map (("arg" ++) . show) [1..numArgs]
  argNames <- mapM newName argStrings
  let argPatList = map VarP argNames
  
  let argTuple = TupE $ map VarE argNames
  cases <- mapM clauseToCase clauseList
  let newBody = NormalB $ CaseE argTuple cases
  let singleClause = Clause argPatList newBody []
  translateDec $ FunD name [singleClause]
  where
    clauseToCase (Clause patList body whereDecs) = do
      let leftSide = TupP patList
      return $ Match leftSide body whereDecs
  

translateDec (ValD pat body whereDecs)  = do
    (ePat, asDecs) <- translatePattern pat
    valBody <- translateBody body
    eWhere <- (asDecs ++) <$> mapM translateDef whereDecs
    let eBody = maybeLet eWhere valBody
    
    return $ single $ D.Definition $ E.Definition ePat (Lo.none eBody) Nothing --TODO what is maybe arg?


translateDec dec@(DataD [] name tyBindings ctors names) = do
    --jsonDecs <- deriveFromJSON defaultOptions name
    eCtors <- mapM translateCtor ctors
    return [ D.Datatype eName eTyVars eCtors []] --TODO derivations?
    where
        eName = nameToElmString name
        eTyVars = map (nameToElmString . tyVarToName) tyBindings


--TODO data case for non-empty context?
translateDec (DataD cxt name tyBindings ctors names) = 
  emitWarning "Data declarations with TypeClass context"

translateDec (NewtypeD cxt name tyBindings  ctor nameList) = unImplemented "Newtypes"

translateDec (TySynD name tyBindings ty) = do
    let eName = nameToElmString name
    let eTyVars = map (nameToElmString . tyVarToName) tyBindings
    eTy <- translateType ty
    return $ single $ D.TypeAlias eName eTyVars eTy []

translateDec (ClassD cxt name tyBindings funDeps decs ) = emitWarning "Class definitions"
translateDec (InstanceD cxt ty decs) = emitWarning "Instance declarations"

--TODO fix signatures
translateDec (SigD name ty) = return []--(single . D.Definition . (E.TypeAnnotation (nameToString name)) ) <$> translateType ty
translateDec (ForeignD frn) = emitWarning "FFI declarations"


translateDec (PragmaD pragma)  = emitWarning "Haskell Pragmas"


translateDec (FamilyD famFlavour name [tyVarBndr] mKind) = emitWarning "Type families"

translateDec (DataInstD cxt name types ctors names) = emitWarning "Data instances"


translateDec (NewtypeInstD cxt name types ctor names) = emitWarning "Newtypes instances"

translateDec (TySynInstD name types theTy) = emitWarning "Type synonym instances"

--------------------------------------------------------------------------
-- | Convert a declaration to an elm Definition
-- Only works on certain types of declarations TODO document which

translateDef :: Dec -> Q E.Def

--TODO functions
translateDef (ValD pat body whereDecs) = do
    (ePat, asDecs) <- translatePattern pat
    eWhere <- (asDecs ++ ) <$> mapM translateDef whereDecs
    decBody <- translateBody body
    let eBody = maybeLet eWhere decBody
    return $ E.Definition ePat (Lo.none eBody) Nothing

translateDef d = unImplemented "Non-simple function/value definitions"

-- | Helper to put an object in a tuple with an empty list as snd
unFst x = (x, [])

--------------------------------------------------------------------------
-- |Translate a pattern match from Haskell to Elm

translatePattern :: Pat -> Q (P.Pattern, [E.Def])
--Special case for As, to carry over the name
translatePattern (AsP name pat) = do
  runIO $ putStrLn "In as-case of translate pattern"
  (retPat, subDecs) <- translatePattern $ pat
  patExp <- patToExp pat
  dec <- translateDef $ ValD (VarP name) (NormalB patExp) []
  return (retPat, [dec] ++ subDecs)
{-
translatePattern p = do
  runIO $ putStrLn $ show p
  ret <-translatePattern' p
  return (ret, [])
  -}

translatePattern (LitP lit) = (unFst . P.PLiteral) <$> translateLiteral lit

translatePattern (VarP name) = return $ unFst $ P.PVar $ nameToElmString name

--Special case: if only one pattern in tuple, don't treat as tuple
--TODO why do we need this?
translatePattern (TupP [pat]) = translatePattern pat

translatePattern (TupP patList) = do
  (patList, allAsDecs) <- unzip <$> mapM translatePattern patList
  return (P.tuple patList, concat allAsDecs)

--Treat unboxed tuples like tuples
translatePattern (UnboxedTupP patList) = translatePattern $ TupP patList  

translatePattern (ConP name patList) = do
  (patList, allAsDecs) <- unzip <$> mapM translatePattern patList
  return (P.PData (nameToElmString name) patList, concat allAsDecs) 

--Just pass through parentheses
translatePattern (ParensP p) = translatePattern p
 
--TODO Infix, tilde, bang, as, record,  view


translatePattern WildP = return $ unFst P.PAnything

--Ignore the type signature if theres one in the pattern
translatePattern (SigP pat _) = translatePattern pat

translatePattern (ListP patList) = do
  (patList, allAsDecs) <- unzip <$> mapM translatePattern patList
  return (P.list patList, concat allAsDecs)

--Convert infix patterns to Data patterns, then let Elm decide
-- how it translates them (i.e. cons is a special case)                                                     
translatePattern (InfixP p1 name p2) = translatePattern $ ConP name [p1,p2]
--treat unboxed infix like infix
translatePattern (UInfixP p1 name p2) = translatePattern $ InfixP p1 name p2

--TODO implement records
translatePattern (RecP _ _) = unImplemented "Record patterns"



translatePattern (TildeP _) = unImplemented "Tilde patterns/laziness notation"
translatePattern (BangP _) = unImplemented "Baing patterns/strictness notation"

translatePattern (ViewP _ _) = unImplemented "View patterns"

--translatePattern p = unImplemented $ "Misc patterns " ++ show p

-------------------------------------------------------------------------
-- | Convert a pattern into an expression
-- Useful for as patterns, so we can do pattern checking as well as multiple naming
patToExp :: Pat -> Q Exp
patToExp p = patToExp' <$> removeWildcards p
  where
    patToExp' (LitP l) = LitE l
    patToExp' (VarP n) = VarE n
    patToExp' (TupP l) = TupE $ map patToExp' l
    patToExp' (UnboxedTupP l) = UnboxedTupE $ map patToExp' l
    patToExp' (ConP n pl) = foldl  AppE (VarE n) (map patToExp' pl) --Apply constructor to each subexp
    patToExp' (InfixP p1 n p2) = InfixE (Just $ patToExp' p1) (VarE n) (Just $ patToExp' p2)
    patToExp' (UInfixP p1 n p2) = UInfixE (patToExp' p1) (VarE n) (patToExp' p2)
    patToExp' (ParensP p) = ParensE $ patToExp' p
    patToExp' (AsP n p) = patToExp' p --TODO ignore name? Should get covered by other translation
    patToExp' WildP = error "Can't use wildcard in expression"
    patToExp' (ListP pList) = ListE $ map patToExp' pList
    patToExp' _ = unImplemented "Complex as-patterns"

-- | Recursively replace wildcards in an exp with new names
-- Useful for as patterns, so we can unbox patterns and re-pack them with a new name
removeWildcards :: Pat -> Q Pat
removeWildcards WildP = do
  name <- newName "arg"
  return $ VarP name
removeWildcards (TupP l) = TupP <$> mapM removeWildcards l
removeWildcards (UnboxedTupP l) = UnboxedTupP <$> mapM removeWildcards l
removeWildcards (ConP n pl) = (ConP n) <$> mapM removeWildcards pl
removeWildcards (InfixP p1 n p2) = do
  ret1 <- removeWildcards p1
  ret2 <- removeWildcards p2
  return $ InfixP ret1 n ret2
removeWildcards (UInfixP p1 n p2) = do
  ret1 <- removeWildcards p1
  ret2 <- removeWildcards p2
  return $ UInfixP ret1 n ret2
removeWildcards (ParensP p) = ParensP <$> removeWildcards p
removeWildcards (AsP n p) = AsP n <$> removeWildcards p --TODO ignore name? Should get covered by other translation
removeWildcards (ListP pList) = ListP <$> mapM removeWildcards pList
removeWildcards p = return p --All other cases, nothing to remove, either simple or unsupported



--------------------------------------------------------------------------
-- |Translate a function body into Elm
translateBody  :: Body -> Q E.Expr
translateBody (NormalB e) = translateExpression e
--Just convert to a multi-way If statement
translateBody (GuardedB guardExpList) = translateExpression $ MultiIfE guardExpList
  


-- | Expression helper function to convert a Var to a String
expressionToString (VarE name) = nameToElmString name

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
  (ePat, asDecs) <- translatePattern pat
  lambdaBody <- translateExpression expBody
  let eBody = maybeLet asDecs lambdaBody
  return $ E.Lambda ePat (Lo.none eBody)

translateExpression (VarE name) =  return $ E.Var $ nameToElmString name

--Just treat constructor as variable --TODO is this okay?
translateExpression (ConE name) = return $ E.Var $ nameToElmString name

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
      getMatch (Match pat body whereDecs) = do
        (ePat, asDecs) <- translatePattern pat
        eWhere <- (asDecs ++ ) <$> mapM translateDef whereDecs
        matchBody <- translateBody body 
        let eBody = maybeLet eWhere matchBody
        return (ePat, Lo.none eBody)

translateExpression (ListE exps) = (E.ExplicitList . map Lo.none) <$> mapM translateExpression exps

--Unboxed infix expression
translateExpression (UInfixE e1 op e2) = do
    eE1 <- translateExpression e1
    eE2 <- translateExpression e2
    let eOp =  expressionToString op
    return $ E.Binop eOp (Lo.none eE1) (Lo.none eE2)

--Infix where we have all the parts, i.e. not a section
--Just translate as unboxed
translateExpression (InfixE (Just e1) op (Just e2)) = 
  translateExpression $ UInfixE e1 op e2
  

translateExpression (InfixE _ _ _) = unImplemented "Operator sections i.e. (+3)"    
    
--Just ignore signature
translateExpression (SigE exp _) = translateExpression exp

--TODO implement ranges
translateExpression e@(ArithSeqE _) = unImplemented $ "Ranges: " ++ show e

translateExpression e@(DoE _) = unImplemented $ "Sugared do notation: " ++ show e

translateExpression e@(CompE _) = unImplemented $ "List comprehensions: " ++ show e

translateExpression e@(RecConE _ _ ) = unImplemented $ "Record construction: " ++ show e

translateExpression e@(RecUpdE _ _ ) = unImplemented $ "Record update: " ++ show e

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
          (VarT name) -> return $ T.Var (nameToElmString name)
          --sum types/ADTs
          (ConT name) -> return $ T.Data (nameToElmString name) [] --TODO what is this list param?
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

          
