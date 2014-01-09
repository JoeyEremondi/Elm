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

import Data.Aeson.TH


import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Variable as V

import Data.List (isPrefixOf)
--import qualified Elm.Haskelm.Json as J
--import qualified Data.Aeson as A

import Language.Haskell.TH.Desugar.Sweeten
import Language.Haskell.TH.Desugar
{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

--import Parse.Expression (makeFunction)

import Control.Applicative

--Main entry point, and the only public function
--TODO make others private
toElm :: String -> [Dec] -> Q (M.Module () ())
toElm name decs = do
  jsonDecs <- makeFromJson decs
  sumDecs <- giantSumType decs
  elmDecs <- concat <$> mapM translateDec (decs ++ jsonDecs ++ sumDecs)
  return $ M.Module [name] [] [] elmDecs --TODO imports/exports?

--TODO remove
unImplemented s = error $ "Translation of the The following haskell feature is not yet implemented: " ++ s

{-|
Haskell to Elm Translations
Most of these functions operate in the Q monad, so that we can
compare against Haskell expressions or types in quotes (see isIntType etc.)

The return value is a list of Elm declarations
TODO return a module?
-}

-- |Stolen from Parse.Expression so we don't have to change any internal Elm code
makeFunction :: [P.Pattern] -> E.LExpr t v -> E.LExpr t v
makeFunction args body@(Lo.L s _) =
  foldr (\arg body' -> Lo.L s $ E.Lambda arg body') body args

-- |Translate a type variable to a name, ignoring its kind
tyVarToName :: TyVarBndr -> Name
tyVarToName (PlainTV n) = n
tyVarToName (KindedTV n _ ) = n

--Abstract out the translation of names to strings
--So that we can modify if need be
--Right now is just a synonym
nameToString :: Name -> String
nameToString name = 
  case (nameModule name) of
    Nothing -> nameBase name--TODO fancier?
    Just (base) -> if isPrefixOf "GHC." base
                      then nameBase name
                      else showName name


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

-- |Helper for putting elements in a list
single :: a -> [a]
single a = [a]

{-|Translate a Haskell declaration into an Elm Declaration
  Currently implemented:
    ADTs

  TODO make a special error for non-supported features
-}

translateDec:: Dec -> Q [D.Declaration () () ]

--TODO translate where decs into elm let-decs
--TODO what about when more than one clause?
translateDec (FunD name [Clause patList body _where])  = do
    let eName = nameToString name
    eBody <- translateBody body
    ePats <- mapM translatePattern patList
    return $ single $ D.Definition $ E.Def (P.PVar eName) (makeFunction ePats (Lo.none eBody))

translateDec (ValD pat body _where)  = do
    eBody <- translateBody body
    ePat <- translatePattern pat
    return $ single $ D.Definition $ E.Def ePat (Lo.none eBody)


translateDec dec@(DataD [] name tyBindings ctors names) = do
    --jsonDecs <- deriveFromJSON defaultOptions name
    eCtors <- mapM translateCtor ctors
    return $ [ D.Datatype eName eTyVars eCtors] 
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
    return $ single $ D.TypeAlias eName eTyVars eTy

translateDec (ClassD cxt name tyBindings funDeps decs ) = unImplemented "Class definitions"
translateDec (InstanceD cxt ty decs) = unImplemented "Instance declarations"

translateDec (SigD name ty) = (single . D.Definition . (E.TypeAnnotation (nameToString name)) ) <$> translateType ty
translateDec (ForeignD frn) = unImplemented "FFI declarations"


translateDec (PragmaD pragma)  = unImplemented "Haskell Pragmas"


translateDec (FamilyD famFlavour name [tyVarBndr] mKind) = unImplemented "Type families"

translateDec (DataInstD cxt name types ctors names) = unImplemented "Data instances"


translateDec (NewtypeInstD cxt name types ctor names) = unImplemented "Newtypes instances"

translateDec (TySynInstD name types theTy) = unImplemented "Type synonym instances"

--------------------------------------------------------------------------
-- | Convert a declaration to an elm Definition
-- Only works on certain types of declarations TODO document which

translateDef :: Dec -> Q (E.Def t v)

--TODO non-empty where?
translateDef (ValD pat body _where) = do
    ePat <- translatePattern pat
    eExp <- translateBody body
    return $ E.Def ePat (Lo.none eExp)

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
translateBody  :: Body -> Q (E.Expr t v)
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
Currently supported: Variables, literals
-}
translateExpression :: Exp -> Q (E.Expr t v)

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

translateExpression (TupE es) = (E.tuple . (map Lo.none)) <$> mapM translateExpression es

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

translateExpression (ListE exps) = (E.ExplicitList . (map Lo.none)) <$> mapM translateExpression exps

--Infix where we have all the parts
translateExpression (InfixE (Just e1) op (Just e2)) = do
    eE1 <- translateExpression e1
    eE2 <- translateExpression e2
    let eOp =  expressionToString op
    return $ E.Binop eOp (Lo.none eE1) (Lo.none eE2)

--Just ignore signature
translateExpression (SigE exp _) = translateExpression exp

--Just ignore signature
translateExpression e = unImplemented $ "Misc expression " ++ (show e)

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

    noQTrans _ = unImplemented "Misc literals"




--------------------------------------------------------------------------
--Type helper functions

int = [t| Int |]
string = [t| String |]
float = [t| Float |]
bool = [t| Bool |]

isIntType t = do
  tint <- int
  --runIO $ putStrLn $ "Checking if int " ++ (show (t == tint))
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
          _ -> unImplemented "misc types"

          
------------------------------------------------------------------------------------
--Helpers to make to and fromJson functions

-- | Build the AST for the base-cases, translating primitive types, lists, tuples, etc.
makeJsonCase0 (jCtor, ctorName) = Match (ConP (mkName jCtor) [] ) (NormalB $ (ConE (mkName ctorName)) ) [] 
makeJsonCase1 (jCtor, varName, ctorName) = Match (ConP (mkName jCtor) [VarP (mkName varName)]) (NormalB $ AppE (ConE (mkName ctorName)) (VarE (mkName varName))) [] 

jsonCase :: [Match]
jsonCase = (map makeJsonCase1 list1) ++ (map makeJsonCase0 list0) ++ [listCase]
  where
    list1 = [--("Array", "lst", "FromJSON_List"), --TODO can do types?
             ("Json.Number", "n", "FromJSON_Float"),
             ("Json.String", "s", "FromJSON_String"),
             ("Json.Boolean", "b", "FromJSON_Bool")]
    list0 = [("Json.Null", "FromJSON_Null")]
    listCase = Match (ConP (mkName "Json.Array") [VarP (mkName "l")]) (NormalB $ AppE (ConE (mkName "FromJSON_List")) (AppE (AppE (VarE (mkName "map")) (VarE (mkName "fromJson"))) (VarE (mkName "l")) )) [] 

{-
[|case json of
Array lst -> FromJson_List $ map fromJson lst
Number n -> FromJson_Int  n --TODO int vs float?
Null -> FromJSON_Null
String s -> FromJson_String s
Boolean b -> FromJson_Bool b

|]
-}

-- | Filter function to test of a dec is a data
isData :: Dec -> Bool
isData (DataD _ _ _ _ _) = True
isData _ = False

-- | Expression for the fromJson function
fromJson :: Exp
fromJson = VarE (mkName "fromJson")

-- | The variable representing the current Json argument
json :: Exp
json = VarE (mkName "json")

jsonPat :: Pat
jsonPat = VarP (mkName "json") 

-- | Variable for the getter function getting the nth variable from a Json
nthVar :: Exp
nthVar = VarE (mkName "nthVar")

-- | Variable for the getter function getting the nth variable from a Json
jsonType :: Exp
jsonType = VarE (mkName "getType")

-- | Variable for the getter function getting the nth variable from a Json
jsonCtor :: Exp
jsonCtor = VarE (mkName "getCtor")

-- | Expression getting the nth subvariable from a JSON object
getNthVar :: String -> Exp
getNthVar nstr = AppE (AppE nthVar json ) (LitE $ StringL nstr)

getType :: Exp
getType = (AppE jsonType json ) 

getCtor :: Exp
getCtor = (AppE jsonCtor json )

-- |The String argument of the JSON "type" property denoting a given ADT
typeString :: Name -> Q String
typeString name = return $ "FromJSON_" ++ nameToString name

-- | name of the function to convert FromJSON to arg type
unTypeName :: Name -> Q Name
unTypeName name = return $ mkName $ "unFromJSON_" ++ nameToString name

-- |The Pattern to unbox a value into its type from the massive sum type
-- | the second argument is the name to bind the value to
unJsonPat :: Name -> Name -> Q Pat
unJsonPat typeName nameToBind = do
  typeCtor <- mkName <$> typeString typeName
  return $ ConP (typeCtor) [VarP nameToBind]
  
        
matchForCtor :: Con -> Q Match        
matchForCtor (NormalC name types) = do
  let matchPat = LitP $ StringL $ nameToString name
  (subNames, subDecs) <- unzip <$> mapM getSubJson (zip (map snd types) [1,2..])
  let body = if null subNames
              then NormalB $ applyArgs subNames ctorExp
              else NormalB $ LetE subDecs (applyArgs subNames ctorExp)
  return $ Match matchPat body []
  where
    ctorExp = ConE name
    applyArgs :: [Name] -> Exp -> Exp
    applyArgs [] accum = accum
    applyArgs (h:t) accum = applyArgs t $ AppE accum (VarE h)

unJsonType :: Type -> Q Exp
unJsonType (ConT name) = do
  argName <- newName "x"
  lambdaPat <- unJsonPat name argName
  let unType = LamE [lambdaPat] (VarE argName)
  return $ (InfixE (Just unType) fnComp (Just fromJson))
  where
    fnComp = VarE $ mkName "."

unJsonType (AppT ListT t) = do
  subFun <- unJsonType t
  let mapVar = VarE $ mkName "mapJson"
  return $ AppE mapVar subFun

--Unpack JSON into a tuple type
--We convert the JSON to a list
--We make a lambda expression which applies the UnFromJSON function to each element of the tuple
unJsonType t
  | isTupleType t = do
      let fnComp = VarE $ mkName "."
      let tList = tupleTypeToList t
      let n = length tList
      --Generate the lambda to convert the list into a tuple
      subFunList <- mapM unJsonType tList
      argNames <- mapM newName (map (("x" ++) . show) [1 .. n])
      let argValues = map VarE argNames
      let argPat = ListP $ map VarP argNames
      let lambdaBody = TupE $ map (\ (fn, arg) -> AppE fn arg) (zip subFunList argValues)
      let lambda = LamE [argPat] lambdaBody
      let makeList = VarE $ mkName "makeList"
      
      return $ InfixE (Just lambda) fnComp (Just makeList)
  | otherwise = do
      test <- isIntType t
      case test of
        True -> do
          let fnComp = VarE $ mkName "."
          argName <- newName "x"
          lambdaPat <- unJsonPat (mkName "Int") argName
          let unType = LamE [lambdaPat] (AppE (VarE (mkName "round")) (VarE argName) )
          return $ (InfixE (Just unType) fnComp (Just fromJson))
        
  
getSubJson :: (Type, Int) -> Q (Name, Dec)
-- We need special cases for lists and tuples, to unpack them
--TODO recursive case
getSubJson (t, n) = do
  funToApply <- unJsonType t
  subName <- newName "subVar"
  let subLeftHand = VarP subName
  let subRightHand = NormalB $ AppE funToApply (getNthVar $ show n)
  return (subName, ValD subLeftHand subRightHand [])
  

matchForType :: Dec -> Q Match
matchForType dec@(DataD _ name _ ctors []) = do
  let matchPat = LitP $ StringL $ nameToString name
  ctorMatches <- mapM matchForCtor ctors
  let typeBody = NormalB $ CaseE getCtor ctorMatches
  jsonName <- newName "typedJson"
  typeCtor <- mkName <$> typeString name
  let typeBodyDec = ValD (VarP $ jsonName) typeBody []
  let ret = AppE (ConE typeCtor) (VarE jsonName)
  let body = NormalB $ LetE [typeBodyDec] ret
  return $ Match matchPat body []
  
makeFromJson :: [Dec] -> Q [Dec]
makeFromJson allDecs = do
  let decs = filter isData allDecs
  typeMatches <- mapM matchForType decs
  let objectBody = NormalB $ CaseE getType typeMatches
  let objectMatch = Match WildP objectBody []
  let body = NormalB $ CaseE json (jsonCase ++ [objectMatch])
  return [ FunD (mkName "fromJson") [Clause [jsonPat] body []] ]

  
giantSumType :: [Dec] -> Q [Dec]
giantSumType allDecs = do
  let decs = filter isData allDecs
  let typeNames = (map getTypeName decs) ++ ( map mkName ["Int", "Float", "Bool", "String"]) --TODO lists?
  
  ctorStrings <- (mapM typeString typeNames)
  let ctorNames = zip typeNames (map mkName ctorStrings)
  let nullCtor = NormalC (mkName "FromJSON_Null") []
  let listCtor = NormalC (mkName "FromJSON_List") [(NotStrict, AppT ListT (ConT $ mkName "FromJSON")) ]
  let ctors = map (\ (typeName, ctorName) -> NormalC ctorName [(NotStrict, ConT typeName)] ) ctorNames
  return [ DataD [] (mkName "FromJSON") [] (ctors ++ [nullCtor, listCtor]) [] ]
    where 
      getTypeName :: Dec -> Name
      getTypeName (DataD _ name _ _ _ ) = name