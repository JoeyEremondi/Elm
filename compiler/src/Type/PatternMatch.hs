{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Type.PatternMatch (patternMatchAnalysis) where

import qualified Reporting.Error

import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import           Control.Monad
import Data.Foldable (foldrM, toList)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import Data.Char (isAlphaNum)

import qualified Nitpick.PatternMatches as PatError

import Control.Monad.Fail
import Control.Monad.Except (ExceptT, MonadError, throwError, runExceptT)

import qualified Data.Maybe as Maybe

import Data.Text (unpack)

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Reporting.Region as R
import qualified Type.Error as ET
import qualified Type.UnionFind as UF

import Control.Monad.Reader as Reader

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import qualified Type.Type as Type
import Control.Monad.Writer

import Data.Semigroup
import Data.Monoid

import qualified Data.List as List

import qualified Reporting.Result as Result

import qualified SetConstraints.Solve as SC

import Data.Index (ZeroBased(..))
import qualified AST.Utils.Shader

import qualified Debug.Trace as Trace

verbose = True

{-# INLINE trace #-}
-- trace = Trace.trace
trace s x = if verbose then (Trace.trace s x) else x

{-# INLINE doLog #-}
doLog s = when verbose $ putStrLn s
-- doLog s = return ()


type Variable = UF.Point (String, Maybe LitPattern) 

newtype Arity = Arity {getArity :: Int} deriving (Show)


deriving instance (Show a) => Show (A.Located a)
deriving instance Show Can.Expr_
deriving instance Show Can.Pattern_
deriving instance Show Can.PatternCtorArg
-- deriving instance Show Can.Union
instance Show Can.Union where
    show _ = "UNION"
instance Show ZeroBased where
    show z = "ZB"
deriving instance Show Can.CtorOpts
deriving instance Show Can.Def
deriving instance Show Can.CaseBranch
deriving instance Show Can.FieldUpdate
instance Show AST.Utils.Shader.Shader where
    show _ = "SHADER"

-- deriving instance Show Can.Pattern_



data TypeEffect_ typeEffect =
    TypeVar N.Name
    | Alias ModuleName.Canonical N.Name [(N.Name, typeEffect)] typeEffect
    -- | Var Variable
    | App ModuleName.Canonical N.Name [typeEffect]
    | Fun typeEffect typeEffect
    | EmptyRecord
    | Record (Map.Map N.Name typeEffect)
    | Unit
    | Tuple typeEffect typeEffect (Maybe typeEffect)
    deriving (Functor, Traversable, Foldable, Show)

instance Show Variable where
    show v = unsafePerformIO $ do
        v' <- UF.repr v
        fst <$> UF.get v'

newtype Fix f = Fix (f (Fix f))

infix 9 :@
data TypeEffect =  (TypeEffect_ TypeEffect) :@ LitPattern
    deriving Show

data LitPattern_ self =
    SetVar_ Variable
    | Ctor_ String [self]
    -- | Proj_ String Arity Int self
    | Top_
    | Bottom_
    | Intersect_ self self
    | Union_ self self
    | Neg_ self
    deriving (Functor, Traversable, Foldable, Show, Eq)

pattern SetVar v = Fix ( SetVar_ v)
pattern Ctor s l = Fix (Ctor_ s l)
-- pattern Proj s a i p = Fix (Proj_ s a i p)
pattern Top = Fix Top_
pattern Bottom = Fix Bottom_
pattern Union x y = Fix (Union_ x y)
pattern Intersect x y = Fix (Intersect_ x y)
pattern Neg x = Fix (Neg_ x)

data Constraint_ self =
    CAnd_ [self]
    | COr_ [self]
    | CImplies_ self self
    | CIff_ self self
    | CSubset_ LitPattern LitPattern
    | CEqual_ LitPattern LitPattern
    | CTrue_
    | CNot_ self
    deriving (Functor, Traversable, Foldable, Show, Eq)

pattern CAnd l = Fix (CAnd_ l)
pattern COr l = Fix (COr_ l)
pattern CImplies x y = Fix (CImplies_ x y)
pattern CIff x y = Fix (CIff_ x y)
pattern CSubset x y = Fix (CSubset_ x y)
pattern CEqual x y = Fix (CEqual_ x y)
pattern CTrue = Fix CTrue_
pattern CNot x = Fix (CNot_ x)

type LitPattern = Fix LitPattern_
type Constraint = Fix Constraint_

instance (Show (f (Fix f))) => (Show (Fix f))
  where
    show (Fix x) = "(" ++ show x ++ ")"
-- deriving instance (Show a) => Show (LitPattern_ a )
-- deriving instance (Show a) => Show (Constraint_ a)

instance (Eq (f (Fix f))) => (Eq (Fix f)) where
    (Fix a) == (Fix b) = a == b  



newtype Safety = Safety {unSafety :: [(Constraint, R.Region, PatError.Context, [Can.Pattern] )]}
    deriving (Monoid, Semigroup)

getSafetyConstrs :: Safety -> [Constraint]
getSafetyConstrs (Safety l) = map (\(c, _, _, _) -> c) l

(====) :: (Subsetable a, Subsetable b) => a -> b -> Constraint
p1 ==== p2 =
    let l1 = toLit p1
        l2 = toLit p2
    in case (l1, l2 ) of
        -- (_,Top) -> Top << l1
        -- (Top, _) ->  Top << l2
        -- (_,Bottom) ->   l1 << Bottom
        -- (Bottom, _) -> l2 << Bottom
        _ -> CEqual l1 l2

cNot :: Constraint -> Constraint
cNot (CNot c) = c
cNot c = CNot c

unions :: (Subsetable a) => [a] -> LitPattern
-- unions [] = Bottom
unions (a : l) = foldr (\ a b ->  (toLit a) `union` b) (toLit a) l

intersects :: (Subsetable a) => [a] -> LitPattern
-- intersects [] = Top
intersects (a : l) = foldr (\ a b ->  (toLit a) `intersect` b) (toLit a) l

union a b =  toLit a `Union` toLit b
intersect a b = toLit a `Intersect` toLit b

class Subsetable a where
    toLit :: a -> LitPattern

instance Subsetable LitPattern where
    toLit = id

instance Subsetable Variable where
    toLit = SetVar

instance Subsetable TypeEffect where
    toLit (_ :@ v) = toLit v

instance Subsetable (LitPattern_ LitPattern) where
    toLit = Fix

(<<) :: (Subsetable a, Subsetable b ) => a -> b -> Constraint
v1 << v2 = CSubset (toLit v1) (toLit v2)

(</<) :: (Subsetable a, Subsetable b ) => a -> b -> Constraint
v1 </< v2 = CNot $ CSubset (toLit v1) (toLit v2)

instance Semigroup Constraint where
    CTrue <> c = c
    c <> CTrue = c
    c1 <> (CAnd cl) = CAnd (c1 : cl)
    c1 <> c2 = CAnd [c1, c2]

instance Monoid Constraint where
    mempty =  CTrue
    mappend = (Data.Semigroup.<>)

data EffectScheme =
    Forall [Variable] TypeEffect Constraint
    deriving (Show)

type Gamma = Map.Map N.Name EffectScheme

monoscheme :: TypeEffect -> LitPattern -> EffectScheme
monoscheme tipe@(_ :@ var) lit =
    Forall [] tipe (var ==== lit)

monoschemeVar :: TypeEffect -> EffectScheme
monoschemeVar tipe = Forall [] tipe CTrue



-- freeConstraintVars :: TypeEffect -> [Variable]
-- freeConstraintVars ty = [] --TODO implement

traverseTE :: (TypeEffect -> TypeEffect) -> TypeEffect -> TypeEffect
traverseTE f (t :@ e) = f ((f <$>  t) :@ e)

typeSubsts :: (Variable -> Variable) -> TypeEffect -> TypeEffect
typeSubsts sub (t :@ e) = (typeSubsts sub <$> t) :@ (litSubsts sub e)

constrSubsts :: (Variable -> Variable) -> Constraint -> Constraint
constrSubsts sub c@(CSubset p1 p2) = trace ("Subbing in subset " ++ show c) $ CSubset (litSubsts sub p1) (litSubsts sub p2)
constrSubsts sub c@(CEqual p1 p2) = trace ("Subbing in equal " ++ show c) $ CEqual (litSubsts sub p1) (litSubsts sub p2)
constrSubsts sub (Fix c) = trace ("Subbing in else " ++ show c) $  Fix (constrSubsts sub <$> c)

litSubsts :: (Variable -> Variable) -> LitPattern -> LitPattern
litSubsts sub l@(SetVar v) = trace ("Subbing in lit var " ++ show l) $  SetVar $ sub v
litSubsts sub (Fix l) = trace ("Subbing in lit else " ++ show l) $  Fix (litSubsts sub <$> l)

-- typeSubsts :: (Variable -> Variable) -> TypeEffect -> TypeEffect
-- typeSubsts sub (t :@ e) = oneTypeSubst sub ((oneTypeSubst sub <$> t) :@ e)

-- constrSubsts :: (Variable -> Variable) -> Constraint -> Constraint
-- constrSubsts sub (Fix c) =  oneConstrSubst sub (Fix $ (oneConstrSubst sub <$> c))

-- litSubsts :: (Variable -> Variable) -> LitPattern -> LitPattern
-- litSubsts sub (Fix l) = oneLitSubst sub (Fix $ (oneLitSubst sub <$> l)) 

typeLocalVars (t :@ e) = litFreeVars e
constrLocalVars (CSubset p1 p2) = (litFreeVars p1) ++ (litFreeVars p2)
constrLocalVars (CEqual p1 p2) = (litFreeVars p1) ++ (litFreeVars p2)
constrLocalVars c = []
litLocalVars (SetVar v) = [v]
litLocalVars l = []

typeFreeVars (t :@ e) =  (litFreeVars e) ++ concatMap typeLocalVars (toList t)
constrFreeVars (Fix c) =  concatMap constrLocalVars ((Fix c) : toList c)
litFreeVars (Fix l) =  concatMap litLocalVars ((Fix l) : toList l)
-- schemeFreeVars (Forall v t c  ) =  (typeFreeVars t) ++ (constrFreeVars c) 

freshName :: (ConstrainM m) => String -> m String
freshName s = do
    ioref <- State.get
    i <- liftIO $ readIORef ioref
    liftIO $ writeIORef ioref (i + 1)
    return $ s ++ "_" ++  show i

varNamed :: (ConstrainM m) => String -> m Variable
varNamed s =
    liftIO $ UF.fresh (s, Nothing)

freshVar :: (ConstrainM m) => m Variable
freshVar = do
    desc <- freshName "SetVar"
    liftIO $ UF.fresh (desc, Nothing) 

instantiate :: (ConstrainM m) => EffectScheme -> m (TypeEffect, Constraint)
instantiate (Forall boundVars tipe constr ) = do
    freshVars <- forM [1 .. length boundVars] $ \ _ -> freshVar
    let subList =  zip boundVars freshVars
    case subList of
        [] -> return (tipe, constr)
        _ -> do
            liftIO $ doLog $ "Instantiating with SubList" ++ (show subList)
            let substFun x = unsafePerformIO $ do
                 equivs <- filterM (UF.equivalent x . fst ) subList
                 case equivs of
                    [] -> do
                        doLog $ "Didn't find variable " ++ (show x) ++ " in list " ++ ( show subList)
                        return x
                    (_,z):[] -> do
                        doLog $ "Replacing " ++ (show x) ++ " with " ++ (show z)
                        return z
                    _ -> error "Too many matching vars in instantiate"
            return $ (typeSubsts substFun tipe, constrSubsts substFun constr)


generalize :: (ConstrainM m) => Gamma -> TypeEffect -> Constraint  -> m EffectScheme
generalize _Gamma tipe constr = do
    let allFreeVars_dupes = (typeFreeVars tipe) ++ constrFreeVars constr
    allFreeVars <- liftIO $ List.nub <$> mapM UF.repr allFreeVars_dupes
    let schemeVars (Forall bnd (t :@ lit) sconstr) = liftIO $ do
            let vEff = litFreeVars lit
            let vrest = constrFreeVars sconstr
            reprs <- mapM UF.repr (vEff ++ vrest)
            boundReprs <- mapM UF.repr bnd
            return $ reprs List.\\ boundReprs
    gammaFreeVars <- (List.nub . concat) <$> mapM schemeVars (Map.elems _Gamma)
    return $  Forall (allFreeVars List.\\ gammaFreeVars) tipe constr

subConstrVars (Fix c) = 
    case c of
        (CSubset_ l1 l2) -> CSubset <$> (subLitVars l1) <*> (subLitVars l2)
        (CEqual_ l1 l2) -> CEqual <$> (subLitVars l1) <*> (subLitVars l2)
        _ -> Fix <$> mapM subConstrVars c
-- subLitVars :: LitPattern -> m LitPattern
subLitVars (Fix l) = case l of
    (SetVar_ v) -> do
        (_,ty) <- liftIO $ UF.get v
        case ty of
            Nothing -> return (SetVar v)
            (Just l) -> subLitVars l
    _ -> Fix <$> mapM subLitVars l 
subTypeVars (t :@ e) = do
    tnew <-  (mapM subTypeVars t) 
    enew <- (subLitVars e)
    return $ tnew :@ enew 

optimizeConstr :: (ConstrainM m) => TypeEffect  -> Constraint -> m (TypeEffect, Constraint)
optimizeConstr tipe (CAnd []) = return (tipe, CTrue)
optimizeConstr tipe (CAnd l) = do
    optimized <- doOpts l
    case (optimized == l) of
        True -> return $ (tipe, CAnd l)
        False -> optimizeConstr tipe (CAnd optimized)
    where
        doOpts l = do 
            liftIO $ doLog ("Initial list:\n" ++ show l)
            lSubbed <- forM l subConstrVars
            liftIO $ doLog ("After subbed:\n" ++ show lSubbed)
            optimized <- helper lSubbed []
            liftIO $ doLog ("After opt:\n" ++ show optimized)
            ret <- forM optimized subConstrVars
            liftIO $ doLog ("After second sub:\n" ++ show ret)
            return $ removeDead ret tipe
        removeDead l tp = 
            let
                occurrences = map constrFreeVars l
                varIsDead v = (not $ v `elem` (typeFreeVars tp)) && (length (filter (v `elem`) occurrences) < 2)
                constrIsDead (CSubset (SetVar v) l) = varIsDead v
                constrIsDead (CSubset l (SetVar v)) = varIsDead v
                constrIsDead (CEqual (SetVar v) l) = varIsDead v
                constrIsDead (CEqual l (SetVar v)) = varIsDead v
                constrIsDead c = False
            in filter (not . constrIsDead) l
        
        -- subVars :: Constraint -> m Constraint

        -- helper :: [Constraint] -> [Constraint] -> m [Constraint] 
        helper [] accum = return $ reverse accum
        helper (CTrue : rest) accum = helper rest accum
        helper ((CEqual (SetVar l1) (SetVar l2)) : rest) accum = do
            eq <- liftIO $ UF.equivalent l1 l2
            case eq of
                True -> helper rest accum
                False -> do
                    constr <- unifyEffectVars l1 l2
                    helper (constr: rest) accum
        helper ((CEqual l1 (SetVar v)): rest) accum = do
            (name, mval) <- liftIO $ UF.get v
            case mval of
                Nothing -> do
                    liftIO $ UF.set v (name, Just l1)
                    helper rest accum
                Just l2 -> helper ((CEqual l1 l2 ) : rest) accum 
        helper ((CEqual (SetVar v) l1 ): rest) accum = do
            (name, mval) <- liftIO $ UF.get v
            case mval of
                Nothing -> do
                    liftIO $ UF.set v (name, Just l1)
                    helper rest accum
                Just l2 -> helper ((CEqual l1 l2 ) : rest) accum
            
        helper ((CSubset (SetVar l1) (SetVar l2)) : (CSubset (SetVar l2') (SetVar l1')) : rest) accum | l1 == l1' && l2 == l2' = do
            desc <- liftIO $ UF.get l1
            liftIO $ UF.union l1 l2 desc
            helper rest accum
        helper ((CSubset _ Top) : l ) accum = helper l accum
        helper (CSubset Top pat@(SetVar _) : rest) accum = helper ((CEqual Top pat):rest) accum
        helper ((CSubset Bottom _) : l) accum = helper l accum
        helper (CSubset pat@(SetVar _) Bottom : rest) accum = helper ((CEqual Bottom pat):rest) accum
        helper ((CAnd l) : rest) accum = helper (l ++ rest) accum
        helper (h : rest) accum = helper rest (h : accum)
optimizeConstr tipe c = return (tipe, c)


toSC :: (ConstrainM m) => Constraint -> m SC.CExpr
toSC c = case c of
    CTrue -> return $ SC.CSubset SC.Top SC.Top
    CAnd cl -> SC.CAnd <$> mapM toSC cl
    COr cl -> SC.COr <$> mapM toSC cl
    CNot c -> SC.CNot <$> toSC c
    (CEqual l1 l2) -> toSC ((CSubset l1 l2) /\ (CSubset l2 l1))
    CSubset l1 l2 -> SC.CSubset <$> toSCLitNoCycle [] l1 <*> toSCLitNoCycle [] l2
    CImplies c1 c2 -> SC.CImplies <$> (toSC c1) <*> (toSC c2)
    CIff c1 c2 -> SC.CIff <$> (toSC c1) <*> (toSC c2)

toSCLitNoCycle :: (ConstrainM m) => [Variable] -> LitPattern -> m SC.Expr
toSCLitNoCycle seen l = do
    let toSCLit  = toSCLitNoCycle seen 
    case l of
        SetVar uf -> do
            ufRepr <- liftIO $ UF.repr uf
            (reprName, reprVal) <- liftIO $ UF.get $ ufRepr
            case (ufRepr `elem` seen || Maybe.isNothing reprVal) of
                True -> return $ SC.Var reprName 
                False -> toSCLitNoCycle (ufRepr : seen) (Maybe.fromJust reprVal)
            
        --Our theory doesn't support projections as expressions 
        --So we generate a fresh variable and constrain that it must contain exactly the projection
        -- Proj ctor arity i l -> do
        --     projName <-  freshName "ProjVar"
        --     tform <- toSCLit l
        --     -- simpleReturn $ SC.Var projName
        --     return $ \ f -> tform $ \ e -> SC.withProjection projName (getArity arity) (SC.Projection ctor (i-1) e) f
        Union l1 l2 ->
            SC.Union <$> toSCLit l1 <*> toSCLit l2
        Intersect l1 l2 -> SC.Intersect <$> toSCLit l1 <*> toSCLit l2
        Neg l1 -> SC.Neg <$> toSCLit l1
        Top -> return $ SC.Top
        Bottom -> return $ SC.Bottom
        Ctor name args -> SC.FunApp name <$> (mapM toSCLit args)
        l -> error $ "Missing case for lit" ++ show l

solveConstraint :: ConstrainM m => Constraint -> m (Either String ())
solveConstraint CTrue = return $ Right ()
solveConstraint c = do
    liftIO $ doLog ("Flattened top level:\n" ++ show c ++ "\n")
    sc <- toSC c
    liftIO $ putStrLn "Solving pattern match constraints"
    ret <- liftIO $ SC.solve (SC.Options "" False "z3" False False False) sc
    liftIO $ putStrLn "Solved Pattern Match constraints"
    return ret

(<==>) ::  Constraint -> Constraint -> Constraint
CTrue <==> y = y
x <==> y =  CIff x y

(==>) ::  Constraint -> Constraint -> Constraint
CTrue ==> y = y
x ==> y =  CImplies x y

--Smart constructor for AND
(/\) :: Constraint -> Constraint -> Constraint
CTrue /\ x = x
x /\ CTrue = x
(CAnd c1) /\  (CAnd c2) =   CAnd (c1 ++ c2)
c1 /\  (CAnd c2) =  CAnd (c1 : c2)
(CAnd c1) /\ c2 =  CAnd (c2 : c1 )
c1 /\ c2 =  CAnd [c1, c2]

(\/) :: Constraint -> Constraint -> Constraint
(COr c1) \/  (COr c2) =   COr (c1 ++ c2)
c1 \/  (COr c2) =  COr (c1 : c2)
(COr c1) \/ c2 =  COr (c2 : c1 )
c1 \/ c2 =  COr [c1, c2]


addEffectVars :: (ConstrainM m) => Can.Type -> m TypeEffect
addEffectVars (Can.TAlias t1 t2 t3 actualType) = case actualType of
    Can.Holey t -> addEffectVars t
    Can.Filled t -> addEffectVars t
addEffectVars t = do
    innerType <- case t of
        (Can.TLambda t1 t2) -> Fun <$> addEffectVars t1 <*> addEffectVars t2
        (Can.TVar n) -> return $ TypeVar n
        (Can.TType t1 t2 t3) -> (App t1 t2) <$> (mapM addEffectVars  t3)
        (Can.TRecord t1 t2) ->
            Record <$> (mapM (\ (Can.FieldType _ t) -> addEffectVars t) t1)
        Can.TUnit -> return $ Unit
        (Can.TTuple t1 t2 t3) -> do
            mt3 <- case t3 of
                Nothing -> return Nothing
                Just t -> Just <$> addEffectVars t
            Tuple <$> addEffectVars t1 <*> addEffectVars t2 <*> (return mt3)
    constraintVar <- SetVar <$> freshVar
    return $ innerType :@ constraintVar
-- addEffectVars (Can.Alias t1 t2 t3 actualType) = addEffectVars actualType
-- addEffectVars t = do
--     innerType <-
--         case t of
--             (Type.PlaceHolder t) ->
--                 return $ PlaceHolder t
--             (Type.VarN t) ->
--                  
--             (Type.AppN t1 t2 t3) ->
--                 (App t1 t2) <$> (mapM addEffectVars  t3)
--             (Type.FunN t1 t2) ->
--                 Fun <$> addEffectVars t1 <*> addEffectVars t2
--             Type.EmptyRecordN -> return EmptyRecord
--             (Type.RecordN t1 t2) ->
--                 Record <$> (mapM addEffectVars t1) <*> addEffectVars t2
--             Type.UnitN -> return Unit
--             (Type.TupleN t1 t2 t3) -> do
--                 mt3 <- case t3 of
--                     Nothing -> return Nothing
--                     Just t -> Just <$> addEffectVars t
--                 Tuple <$> addEffectVars t1 <*> addEffectVars t2 <*> (return mt3)
--     

-- freshTE :: IO TypeEffect
-- freshTE = do
--     tipe <- Var <$> freshVar --TODO what descriptor?
--     effect <- freshVar --TODO what descriptor?
--     return $ tipe :@ effect

unifyEffects :: (ConstrainM m) => LitPattern -> LitPattern -> m Constraint
unifyEffects (SetVar v1) (SetVar v2) = unifyEffectVars v1 v2
unifyEffects (SetVar v) l1 = do
    (name, ml2 ) <- liftIO $ UF.get v
    case ml2 of
        Just l2 -> return $ l1 ==== l2
        Nothing -> do
            liftIO $ UF.set v (name, Just l1)
            return CTrue
unifyEffects l1 (SetVar v) = do
    (name, ml2 ) <- liftIO $ UF.get v
    case ml2 of
        Just l2 -> return $ l1 ==== l2
        Nothing -> do
            liftIO $ UF.set v (name, Just l1)
            return CTrue 
unifyEffects l1 l2 = return (l1 ==== l2)

unifyEffectVars :: (ConstrainM m) => Variable -> Variable -> m Constraint
unifyEffectVars v1 v2 = do
    (name1, eff1) <- liftIO $ UF.get v1
    (name2, eff2) <- liftIO $ UF.get v2
    case (eff1, eff2) of
        (Nothing, _) -> do
            liftIO $ UF.union v1 v2 (name2, eff2)
            return CTrue
        (_,Nothing) -> do
            liftIO $ UF.union v1 v2 (name1, eff1)
            return CTrue
        (Just e1, Just e2) -> do
            liftIO $ UF.union v1 v2 (name1, Just e1)
            return (e1 ==== e2) 


unifyTypes :: (ConstrainM m) => TypeEffect -> TypeEffect -> m Constraint
unifyTypes (t1 :@ v1) (t2 :@ v2) = do
    constr1 <- unifyEffects v1 v2
    constr2 <- case (t1, t2) of
        ((TypeVar v1), _) -> return CTrue
        (_, (TypeVar v1)) -> return CTrue
        ((Alias t1 t2 t3 t4), _) -> return CTrue --TODO alias unify
        ((App t1 t2 t3), (App t1' t2' t3')) ->
            CAnd <$> zipWithM  unifyTypes t3 t3'
        ((Fun t1 t2), (Fun t1' t2')) -> do
            c1 <- unifyTypes t1 t1'
            c2 <- unifyTypes t2 t2'
            return (c1 /\ c2)
        (EmptyRecord, EmptyRecord) -> return CTrue
        ((Record fields), (Record fields')) -> 
            CAnd <$>( forM (Map.keys fields) $ \key ->
                unifyTypes (fields Map.! key) (fields' Map.! key))
        (Unit, Unit) -> return CTrue
        ((Tuple t1 t2 mt3), (Tuple t1' t2' mt3')) -> do
            c1 <- unifyTypes t1 t1'
            c2 <- unifyTypes t2 t2' 
            c3 <- case (mt3, mt3') of
                (Nothing, Nothing) -> return CTrue
                (Just t3, Just t3') -> unifyTypes t3 t3'
            return (c1 /\ c2 /\ c3)
        _ -> error $ "Can't unify types " ++ (show t1) ++ " and " ++ (show t2 )
    return (constr1 /\ constr2)


type ConstrainM m = (State.MonadState (IORef Int) m, MonadIO m, MonadWriter Safety m, MonadError PatError.Error m )

eitherToPatError :: (ConstrainM m) => m (Either String a) -> m a
eitherToPatError comp = do
    eitherVal <- comp
    case eitherVal of
        Right x -> return x
        Left s -> error s
        --PatError.Incomplete _ PatError.BadArg [Pattern] 

unpackEither :: (ConstrainM m ) => m (Either PatError.Error b) -> m b
unpackEither ec = do
    e <- ec
    case e of
        Left s -> throwError s
        Right b -> return b

newtype ConstrainMonad a = CM {
    runCM :: ExceptT PatError.Error  (WriterT Safety (StateT (IORef Int) IO)) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadWriter Safety, MonadError PatError.Error, State.MonadState (IORef Int))

runCMIO :: IORef Int -> ConstrainMonad a ->  IO (Either PatError.Error (a, Safety))
runCMIO ioref c = do
    (maybeResult, safety) <- flip State.evalStateT ioref $  runWriterT $ runExceptT $ runCM c
    return $ do
        result <- maybeResult
        return (result, safety)

tellSafety pathConstr x r context pats = tell $ Safety [(pathConstr ==> x, r, context, pats)]

--Given a type and an  effect-variable for each expression,
-- a mapping (Gamma) from free variables to effect schemes,
--A "path constraint" of truths that must hold for us to reach this point in the program,
-- and an expression,
--Traverse that expression and generate the constraints that, when solved, 
-- give the possible patterns for each effect variable.
-- Emits "safety constraints" using the Writer monad
constrainExpr :: (ConstrainM m) => Map.Map R.Region TypeEffect -> (Gamma, Constraint) -> Can.Expr ->   m (TypeEffect,  Constraint)
constrainExpr tyMap _GammaPath (A.At region expr)  = do
    liftIO $ doLog ("Constraining exprssion " ++ show expr)
    case Map.lookup region tyMap of
        Just ty -> do
            c <- constrainExpr_ expr ty _GammaPath
            return (ty, c)
        Nothing -> error "Region not in type map"
  where
    self :: (ConstrainM m) => (Gamma, Constraint) -> Can.Expr ->   m (TypeEffect,  Constraint)
    self  = constrainExpr tyMap
    constrainExpr_ ::  (ConstrainM m) => Can.Expr_ -> TypeEffect -> (Gamma, Constraint) -> m Constraint
    constrainExpr_ (Can.VarLocal name) t (_Gamma, pathConstr) = do
        let sigma =
                case Map.lookup name _Gamma of
                    Nothing -> error $ "constrainExpr: name " ++ (N.toString name) ++ " not found in " ++ (show _Gamma)
                    (Just s) -> s
        (tipe, constr) <- instantiate sigma
        liftIO $ doLog $ "Instantiating " ++ (show sigma) ++ " into " ++ (show (tipe, constr)) ++ " for var " ++ (N.toString name)
        liftIO $ doLog $ "Unifying types" ++ (show t) ++ "\n  and " ++ show tipe
        unifyTypes t tipe
        return constr
    constrainExpr_ e@(Can.VarTopLevel _ name) t (_Gamma, pathConstr) = --TODO what about other modules?
        case Map.lookup name _Gamma  of
            Nothing -> do
                liftIO $ doLog ("TOP for Setting type for " ++ (show e) ++ " imported, var " ++ show t)
                return $ t ==== Top
            Just sigma -> do
                --TODO reduce duplication
                (tipe, constr) <- instantiate sigma
                liftIO $ doLog $ "Instantiating " ++ (show sigma) ++ " into " ++ (show (tipe, constr)) ++ " for var " ++ (N.toString name)
                liftIO $ doLog $ "Unifying types" ++ (show t) ++ "\n  and " ++ show tipe
                unifyTypes t tipe
                return constr
    constrainExpr_ (Can.VarCtor _ _ ctorName _ _) t _GammaPath =
      --Traverse the simple type of the constructor until it's not an arrow type
      --Collect the argument patterns, then apply the Ctor name to those patterns
      return $ ctorLoop t [] CTrue
        where
            ctorLoop ((Fun dom cod) :@ v3) reverseAccum condAccum = ctorLoop cod (dom : reverseAccum) (condAccum /\ (v3 ==== litLambda))
            ctorLoop (_ :@ ctorVar ) reverseAccum condAccum = condAccum /\ (ctorVar ==== (Ctor (N.toString ctorName) (map toLit $ reverse reverseAccum)) )
    constrainExpr_ e@(Can.VarOperator _ _ _ _) t _GammaPath = do
        liftIO $ doLog ("TOP for var operator " ++ show e ++ ", var " ++ show t)
        return $ t ==== Top
    constrainExpr_ e@(Can.Binop name _ _ _ e1 e2) t _GammaPath = do
        (ty1, constr1) <- self _GammaPath e1
        (ty2, constr2) <- self _GammaPath e2
        opConstr <- case N.toString name of
            "::" -> return $ t ==== litCons ty1 ty2
            _ -> do
                liftIO $ doLog ("TOP for binary operator " ++ show e ++ ", var " ++ show t)
                return (t ==== Top)
        return $ constr1 /\ constr2 /\ opConstr --TODO built-in types for operators  
    constrainExpr_ (Can.Case discr branches) resultType (_Gamma, pathConstr) = do
        (inputPatterns, inputConstrs) <- self (_Gamma, pathConstr) discr
        --TODO negate previous branches
        let litBranches = map (\ (Can.CaseBranch pat rhs) -> (pat, canPatToLit pat, rhs) ) branches
        --Emit a safety constraint: must cover all possible inputs by our branch patterns
        tellSafety pathConstr (inputPatterns << unions (map (\(_,b,_) -> b) litBranches)) region PatError.BadCase (map (\(a,_,_)->a) litBranches)
        (patVars, branchConstrs) <- unzip <$>
            forM litBranches (
                \(pat, lit, rhs) -> do
                    v <- freshVar
                    let canBeInBranch = ( toLit inputPatterns `intersect` lit) </< Bottom
                    (newEnv, newEnvConstr) <- envAfterMatch tyMap (toLit inputPatterns) pat
                    let newPathConstr = canBeInBranch /\ pathConstr
                    (rhsTy, rhsConstrs) <- self (Map.union newEnv _Gamma, newPathConstr) rhs
                    return (v,
                        newEnvConstr /\ rhsConstrs /\ (canBeInBranch ==> (v ==== rhsTy))
                        /\ ((cNot canBeInBranch) ==> (v ==== Bottom))))
        --The result of the whole thing contains the union of all the results of the branches
        --We set this as << in case we've lost information due to recursion
        let resultConstr = resultType << (unions patVars)
        return (inputConstrs /\ resultConstr /\ CAnd branchConstrs)
    --Lambda base case: just typecheck the body if there are 0 args

    constrainExpr_ (Can.Lambda allArgPats body) t (_Gamma, pathConstr) =
        lamHelper allArgPats t (_Gamma, pathConstr)
            where
                --TODO need to alter path condition?
                lamHelper [] t _GammaPath = do
                    --TODO need to unify types more?
                    (bodyType, bodyConstr) <- self _GammaPath body
                    unifyTypes t  bodyType
                    return bodyConstr
                lamHelper (argPat:argPats) t@( (Fun dom cod) :@ v3 ) (_Gamma, pathConstr) = do
                    --Emit a safety constraint saying that the argument must match the pattern
                    let litPat = canPatToLit argPat
                    tellSafety pathConstr (dom << litPat) region PatError.BadArg [argPat]
                    --All values of function types must be lambdas, so we have a trivial constraint on v3
                    let lamConstr = (v3 ==== litLambda)
                    --Get the environment to check the body
                    (newEnv, newEnvConstr) <- envAfterMatch tyMap (toLit dom) argPat
                    --Check the body --TODO path constr?
                    bodyConstr <- lamHelper argPats cod (Map.union newEnv _Gamma, pathConstr)
                    return $ newEnvConstr /\ bodyConstr /\ lamConstr
    constrainExpr_ (Can.Call fun args) retEffect _GammaPath@(_Gamma, pathConstr) = do
         (funTy, funConstr) <- self _GammaPath fun
         (argTEs, argConstrs) <- unzip <$> mapM (self _GammaPath) args
         ret <- argHelper funTy argTEs ( funConstr /\ CAnd argConstrs)
         liftIO $ doLog ("Function call " ++ show fun ++ "\n    generates constr " ++ show ret)
         return ret
            where
                --Loop to compute the return type
                --At each application, instantiate the function's argument type to be the type of the given argument
                --Finally, make sure the patterns of the whole expression is the pattern the function returns
                argHelper funEffect [] accum =
                    return $  (funEffect << retEffect) /\ accum
                argHelper (Fun dom cod :@ _) (argTy : rest) accum = do
                    liftIO $ doLog $ "Constraining that argTy " ++ (show argTy) ++ ("Smaller than " ++ show dom)
                    tellSafety pathConstr (argTy << dom) region PatError.BadArg [] --TODO something sensible for pat list here 
                    argHelper cod rest accum
    constrainExpr_ (Can.If conds elseExpr) t _GammaPath = do
        (elseType, elseCond) <- self _GammaPath elseExpr
        (branchTypes, branchConds) <-
            unzip <$>
                forM conds (
                    \ (ifExp, thenExp) -> do
                        retType <- freshVar
                        (ifType, ifCond) <- self _GammaPath ifExp
                        (thenType, thenCond) <- self _GammaPath thenExp
                        return (retType,
                            ifCond
                            /\ thenCond
                            /\ ((litTrue << ifType ) ==> (retType ==== thenType) )
                            /\ ((((litTrue `intersect` (toLit ifType)) ==== Bottom)) ==> (retType ==== Bottom))))
        return $ elseCond /\ (CAnd branchConds) /\ (t ==== unions ((toLit elseType) : (map toLit branchTypes)))
    constrainExpr_ (Can.Let def inExpr)  t _GammaPath@(_Gamma, pathConstr) = do
        --We don't generate any constraints when we constrain a definition
        --Because those constraints get wrapped up in the EffectScheme
        --And are instantiated at each use of x
        envExt <- constrainDef tyMap _GammaPath def
        (bodyType, bodyConstr) <- self (Map.union envExt _Gamma, pathConstr) inExpr
        unifyTypes bodyType t
        return bodyConstr
    constrainExpr_ (Can.LetDestruct pat letExp inExp) t _GammaPath@(_Gamma, pathConstr) = do
        --TODO need to generalize?
        let lit = canPatToLit pat
        --Can't have a recursive let on a pattern-match, since won't be behind a lambda
        --TODO is this right?
        (letType, letConstr) <- self _GammaPath letExp
        --Safety constraint: must match whatever we are binding
        tellSafety pathConstr (letType << lit) region PatError.BadCase [pat]
        (envExt, envExtConstr) <- envAfterMatch tyMap (toLit letType) pat
        --TODO extend path cond
        (bodyType, bodyConstr) <- self (Map.union envExt _Gamma, pathConstr) inExp
        --Whole expression has type of body
        unifyTypes bodyType t
        return $ envExtConstr /\ letConstr /\ bodyConstr
    constrainExpr_ (Can.Accessor expr) t _GammaPath = return $ t ==== Top
    constrainExpr_ (Can.Access expr1 expr2) t _GammaPath = return $ t ==== Top
    constrainExpr_ (Can.Update expr1 expr2 expr3) t _GammaPath = return $ t ==== Top
    constrainExpr_ (Can.Record expr) t _GammaPath = return $ t ==== Top
    constrainExpr_ (Can.Tuple expr1 expr2 Nothing) (_ :@ vTuple) _GammaPath = do
        (elemEffect1, constr1) <- self _GammaPath expr1
        (elemEffect2, constr2) <- self _GammaPath expr2
        return $ CAnd [constr1, constr2, (litPair elemEffect1 elemEffect2) ==== vTuple]
    constrainExpr_ (Can.Tuple expr1 expr2 (Just expr3)) (_ :@ vTuple) _GammaPath = do
        (elemEffect1, constr1) <- self _GammaPath expr1
        (elemEffect2, constr2) <- self _GammaPath expr2
        (elemEffect3, constr3) <- self _GammaPath expr3
        return $ CAnd [constr1, constr2, constr3, (litTriple elemEffect1 elemEffect2 elemEffect3) ==== vTuple]
    constrainExpr_ (Can.List expr) t _GammaPath = do
        (typeEffects, constrs) <- unzip <$> forM expr (self _GammaPath)
        return $ (CAnd constrs) /\ (t ==== litList typeEffects )
    constrainExpr_ (Can.Chr c) typeEffect _GammaPath = return $ (litChar c) ====  typeEffect
    constrainExpr_ (Can.Str s) typeEffect  _GammaPath = return $ (litString s) ==== typeEffect
    constrainExpr_ (Can.Int i) typeEffect  _GammaPath =  return $ (litInt i) ==== typeEffect
    constrainExpr_ (Can.Float expr) t _GammaPath = return CTrue
    constrainExpr_ (Can.Negate expr) t _GammaPath = return (t ==== Top) --TODO something smart for negation
    constrainExpr_ e@(Can.VarKernel expr1 expr2) t _GammaPath = do
        liftIO $ doLog ("TOP for varKernel " ++ show e ++ ", var " ++ show t)
        return (t ==== Top)
    constrainExpr_ (Can.VarForeign expr1 expr2 expr3) t _GammaPath = return (t ==== Top)
    constrainExpr_ (Can.VarDebug expr1 expr2 expr3) t _GammaPath = return (t ==== Top)
    constrainExpr_ Can.Unit (_:@v) _GammaPath = return $ litUnit ==== v
    constrainExpr_ (Can.Shader expr1 expr2 expr3) t _GammaPath = return (t ==== Top )
    constrainExpr_ _ _ _ = error $ "Impossible type-expr combo "


--Takes place in the IO monad, not our ConstrainM
--Because we want to generate a separate set of safety constraints for this definition
--TODO check all this
constrainDef :: (ConstrainM m) => Map.Map R.Region TypeEffect -> (Gamma, Constraint) -> Can.Def ->   m Gamma
constrainDef tyMap _GammaPath@(_Gamma, pathConstr) def = do
    theRef <- State.get
    (x, defType, defConstr, safety) <- case def of
        --Get the type of the body, and add it into the environment as a monoscheme
        --To start our argument-processing loop
        (Can.Def (A.At wholeRegion x) funArgs body) -> do
            let wholeType =
                    case Map.lookup wholeRegion tyMap of
                        Nothing -> error $ "constrainDef: Can't find region " ++ (show wholeRegion) ++ " in type map " ++ (show tyMap)
                        Just s-> s
            --We run in a separaelm te instance, so we get different safety constraints
            --TODO need this?
            (exprConstr, safety) <- unpackEither $ liftIO $ runCMIO theRef $ constrainDef_  funArgs body wholeType (Map.insert x (monoschemeVar wholeType) _Gamma)
            return (x, wholeType, exprConstr, safety)
        (Can.TypedDef (A.At wholeRegion x) _ patTypes body retTipe) -> do
            retTyEff <- addEffectVars retTipe
            argTyEffs <- mapM (addEffectVars . snd) patTypes
            freshVars <- forM patTypes $ \ _ -> SetVar <$> freshVar
            let wholeType = foldr (\ (arg, var) ret -> (Fun arg ret) :@ var) retTyEff (zip argTyEffs freshVars)
            -- let wholeType = 
            --         case Map.lookup wholeRegion tyMap of
            --             Nothing -> error $ "constrainDef typed: Can't find region " ++ (show wholeRegion) ++ " in type map " ++ (show tyMap)
            --             Just s-> s 
            --We run in a separate instance, so we get different safety constraints
            --TODO need this?
            (exprConstr, safety)  <- unpackEither $ liftIO $ runCMIO theRef $ constrainDef_  (map fst patTypes) body wholeType (Map.insert x (monoschemeVar wholeType) _Gamma)
            return (x, wholeType, exprConstr, safety)
    --We check that each safety constraint in this definition is compatible with the other constraints
    let safetyList = unSafety safety
    liftIO $ doLog $  "Solving constraints for definition " ++ N.toString  x
    liftIO $ doLog $ "Got safety constraints " ++ (show $ getSafetyConstrs safety)
    (optimizedType , optimizedConstr) <- optimizeConstr defType (defConstr /\ CAnd (getSafetyConstrs safety)) 
    mConstraintSoln <- solveConstraint optimizedConstr
    case mConstraintSoln of
        Right () -> return ()
        Left _ -> do
            error "Pattern match failure"
            failures <- forM safetyList $ \(safetyConstr, region, context, pats) -> do
                soln <- solveConstraint (defConstr /\ safetyConstr)
                case soln of
                    Right _ -> return $ Nothing
                    Left _ -> return $ Just (region, context, pats) --TODO get unmatched patterns
            case Maybe.catMaybes failures of
                [] -> error "Incompatible safety constraint set"
                l -> forM_ l $ \(region, context, pats) ->
                    throwError $ PatError.Incomplete region context (map PatError.simplify pats )
            return ()
            --Iterate through each safety constraint, and see which one is satisfiable

    --Now that we have types and constraints for the body, we generalize them over all free variables
    --not  occurring in Gamma, and return an environment mapping the def name to this scheme
    scheme <- generalize (fst _GammaPath) optimizedType optimizedConstr
    liftIO $ doLog $ "Generalized type for " ++ N.toString x ++ " is " ++ (show scheme)
    return $ Map.singleton x scheme
    where
        constrainDef_  (argPat : argList) body ((Fun dom cod) :@ vFun) _Gamma = do
            --Add the argument pattern vars to the dictionary, then check the rest at the codomain type
            (envExt, envExtConstr) <- (envAfterMatch tyMap (toLit dom) argPat)
            retConstr <- constrainDef_ argList body cod (Map.union envExt  _Gamma)
            return (envExtConstr /\ retConstr)
        constrainDef_  [] body exprType _Gamma = do
            (bodyType, bodyConstr) <- constrainExpr tyMap (_Gamma, pathConstr) body
            unifyTypes bodyType exprType
            --Now that we have the type and constraints for our definition body
            --We can generalize it into a type scheme and return a new environment 
            --TODO run constraint solver at this point
            return bodyConstr
        constrainDef_ argList body t _Gamma = error ("Got bad type " ++ show t ++ " for def num args " ++ show (length argList) )

--Convert to our pattern format
--Used to generate safety constraints for pattern matches   
canPatToLit ::  Can.Pattern -> LitPattern
canPatToLit  (A.At info pat) =
    case pat of
        Can.PAnything -> Top
        (Can.PVar x) -> Top
        (Can.PRecord p) -> error "TODO records"
        (Can.PAlias p1 p2) -> error "TODO Pattern alias"
        Can.PUnit -> litUnit
        (Can.PTuple p1 p2 (Just p3)) -> litTriple (canPatToLit p1) (canPatToLit p2) (canPatToLit p3)
        (Can.PTuple p1 p2 Nothing) -> litPair (canPatToLit p1) (canPatToLit p2)
        (Can.PList plist) -> litList (map canPatToLit plist)
        (Can.PCons p1 p2) -> litCons (canPatToLit p1) (canPatToLit p2)
        (Can.PBool _ b) -> if b then litTrue else litFalse
        (Can.PChr c) -> litChar c
        (Can.PStr s) -> litString s
        (Can.PInt i) -> litInt i
        Can.PCtor { Can._p_name = ctorName, Can._p_args = ctorArgs } -> Ctor (N.toString ctorName) (map (canPatToLit . Can._arg) ctorArgs)

getProjections :: (ConstrainM m) => String -> Arity -> LitPattern -> m (Constraint, [LitPattern])
getProjections name arity pat = do
    projName <- freshName "projVar"
    let argnums = [1 .. getArity arity]
    projPatterns <- forM argnums $ \i -> SetVar <$> varNamed (projName ++ "_" ++ show i)
    let partMatchingCtor = pat `intersect` (Ctor name (replicate (getArity arity) Top))
    let varsMatchProj = (Ctor name projPatterns) ==== partMatchingCtor
    let emptyIff = CAnd $ map (\proj -> (proj ==== Bottom) <==> (partMatchingCtor ==== Bottom)) projPatterns
    return (varsMatchProj /\ emptyIff, projPatterns)


envAfterMatch :: (ConstrainM m) => Map.Map R.Region TypeEffect ->  LitPattern  -> Can.Pattern -> m (Gamma, Constraint)
envAfterMatch tyMap matchedPat (A.At region pat)  = do
    let
        ourType =
            case Map.lookup region tyMap of
                Nothing -> error $ "envAfterMatch: Can't find region " ++ (show region) ++ " in type map " ++ (show tyMap)
                Just s-> s
    let ctorProjectionEnvs  topPat nameString ctorArgPats = do
            let arity = Arity (length ctorArgPats)
            (projConstrs, subPats) <- getProjections nameString arity topPat
            (subDicts, subConstrs) <- unzip <$> zipWithM  ((envAfterMatch tyMap) ) subPats ctorArgPats
            return $ (Map.unions subDicts, projConstrs /\ CAnd subConstrs)
    case pat of
        Can.PVar x -> return $ (Map.singleton x (monoscheme ourType matchedPat), CTrue)
        Can.PCtor { Can._p_name = ctorName, Can._p_args = ctorArgs } ->
            ctorProjectionEnvs matchedPat (N.toString ctorName) $ map Can._arg ctorArgs
        (Can.PTuple p1 p2 (Just p3)) -> ctorProjectionEnvs matchedPat ctorTriple [p1, p2, p3]
        (Can.PTuple p1 p2 Nothing) -> ctorProjectionEnvs matchedPat ctorPair [p1, p2]
        (Can.PList []) -> return $ (Map.empty, CTrue) --Nothing to add to env for Null
        --For lists, we get split into head and tail and handle recursively
        (Can.PList (p1 : pList)) -> ctorProjectionEnvs matchedPat ctorCons [p1, A.At region (Can.PList pList)]
        (Can.PCons p1 p2) -> ctorProjectionEnvs matchedPat ctorCons [p1, p2]
        _ -> return $ (Map.empty, CTrue)



--Helpers for generating literal patterns from builtin data types
ctorLam = "--Lambda"
litLambda = Ctor ctorLam []
ctorUnit = "--Unit"
litUnit = Ctor ctorUnit []

ctorPair = "--Pair"
litPair l1 l2 = Ctor ctorPair  [(toLit l1), (toLit l2)]
ctorTriple = "--Triple"
litTriple l1 l2 l3 = Ctor ctorTriple  (map toLit [l1, l2, l3])

litList l = case l of
    [] -> litNull
    (h : t) -> litCons h (litList t)
ctorNull = "--Null"
ctorCons = "--Cons"
litNull = Ctor ctorNull []
litCons h t = Ctor ctorCons [toLit h, toLit t]

ctorTrue = "--True"
litTrue = Ctor ctorTrue []
ctorFalse = "--False"
litFalse = Ctor ctorFalse []
ctorChar c = "--CHAR_" ++ unpack c
litChar c = Ctor (ctorChar c) []
ctorString s = "--STRING_" ++ (filter isAlphaNum $ show s)
litString s = Ctor (ctorString s) []

-- ctorZero = "--ZERO"
-- ctorPos = "--POSITIVE"
-- ctorNeg = "--NEGATIVE"
-- ctorSucc = "--SUCC"
litInt i = Ctor ("--_" ++ show i) []
-- litInt i = case i of
--     0 -> Ctor ctorZero []
--     _ | i < 0 -> Ctor ctorPos [litNat (-i)]
--     _ | i > 0 -> Ctor ctorNeg [litNat i]

-- litNat n = case n of
--     0 -> Ctor ctorZero []
--     i | i > 0 -> Ctor ctorSucc [litNat (i-1)]

constrainRecursiveDefs :: (ConstrainM m) => Map.Map R.Region TypeEffect -> Gamma -> [Can.Def] -> m Gamma
constrainRecursiveDefs tyMap _Gamma defs = do
    let ourType region =
            case Map.lookup region tyMap of
                Nothing -> error $ "constrainRecursiveDefs: Can't find region " ++ (show region) ++ " in type map " ++ (show tyMap)
                Just s-> s
    defTypes <- forM defs $ \ def ->
        case def of
            (Can.Def (A.At region name) def2 def3) -> return (name, monoschemeVar (ourType region))
            (Can.TypedDef (A.At wholeRegion name) _ patTypes body retTipe) -> do
                retTyEff <- addEffectVars retTipe
                argTyEffs <- mapM addEffectVars (map snd patTypes)
                freshVars <- forM patTypes $ \ _ -> SetVar <$> freshVar
                let wholeType = foldr (\ (arg, var) ret -> (Fun arg ret) :@ var) retTyEff (zip argTyEffs freshVars)
                return  (name, monoschemeVar wholeType)

    let exprs =
            map (\ def -> case def of
                Can.Def (A.At region _) _ body -> (A.At region body)
                Can.TypedDef (A.At region _) _ _ body _ -> (A.At region body))
    let _Gamma = Map.fromList defTypes
    constrs <- forM defs $ constrainDef tyMap (_Gamma, CTrue)
    return _Gamma

patternMatchAnalysis :: Can.Module -> Result.Result i w Reporting.Error.Error ()
patternMatchAnalysis modul = do
    let
        eitherResult =  unsafePerformIO $ do
            theRef <- newIORef 0
            runCMIO theRef $ do
                tyMapRaw <- liftIO $ readIORef Type.globalTypeMap
                tyMap <- mapM addEffectVars tyMapRaw
                helper tyMap (Can._decls modul) Map.empty
    case eitherResult of
        Left patError -> Result.throw $ Reporting.Error.Pattern [patError]
        Right _ -> return ()
  where
    -- helper ::   _ -> _ -> _ -> ConstrainMonad   _ 
    helper tyMap (Can.Declare def decls)  _Gamma = do
        envExt <- constrainDef tyMap (_Gamma, CTrue) def
        helper tyMap decls (Map.union envExt _Gamma)
    helper tyMap (Can.DeclareRec defs decls)  _Gamma = do
        newGamma <- constrainRecursiveDefs tyMap _Gamma defs
        helper tyMap decls newGamma
    helper theTyMap Can.SaveTheEnvironment  _Gamma =
        return ()