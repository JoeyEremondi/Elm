module Optimize.ForwardSlicing (removeDeadCodeWP, removeDeadCodeModule) where

import Elm.Compiler.Module
import qualified Data.Map as Map
import Optimize.Traversals
import qualified AST.Module as Module
import qualified AST.Expression.Canonical as Canon
import AST.Annotation (Annotated(..))
import AST.Expression.General

import Optimize.Types

--Our different types of control nodes
data ControlNode =
  Start (LabeledExpr)
  | End (LabeledExpr)
  | Call (LabeledExpr)
  | Return (LabeledExpr)
  | ProcEntry (LabeledExpr)
  | ProcExit (LabeledExpr)
  | GlobalEntry --Always the first node
  | GlobalExit --Always the last node


functionName :: LabeledExpr -> Maybe Var
functionName (A _ e) = case e of
  Var v -> Just v
  (App f _ ) -> functionName f
  _ -> Nothing

argsGiven :: LabeledExpr -> Maybe [LabeledExpr]
argsGiven (A _ e) = case e of
  Var v -> Just []
  (App f e ) -> ([e]++) `fmap` argsGiven f
  _ -> Nothing
  
--Generate control-flow edges for a single expression
--We then pass this function to a fold, which collects them
oneLevelEdges
  :: Map.Map Var Int
  -> Map.Map Var (ControlNode, ControlNode) --Map of entry and exit nodes
  -> LabeledExpr
  -> Maybe [(ControlNode, ControlNode)]
oneLevelEdges aritys fnNodes e@(A (_,_,env) e') =
  case e' of
    (Range e1 e2) -> return [(Start e, Start e1), (End e1, Start e2), (End e2, End e)]
    (ExplicitList l) ->
      let
        startList = [Start e] ++ map Start l
        endList = (map End l) ++ [End e]
      in return $ zip startList endList
    (Binop op e1 e2) -> return [(Start e, Start e1), (End e1, Start e2), (End e2, End e)]
    (Lambda e1 e2) -> Nothing --TODO handle this case? Initial level?
    (App e1 e2) -> do
      fnName <- functionName e1
      argList <- argsGiven e1
      let numArgs = length argList
      arity <- Map.lookup fnName aritys
      let inLocalScope = Map.member fnName env
      --TODO check for shadowing?
      case (arity == numArgs, inLocalScope) of
        (True, False) -> do
          (pentry, pexit) <- Map.lookup fnName fnNodes
          let starts = [Start e] ++ map Start argList
          let ends = (map End argList) ++ [Call e]
          return $ [(Call e, pentry), (pexit, Return e)] ++ (zip starts ends)
        _ -> Nothing --If function is locally defined, or not fully instantiated, we fail
    (MultiIf guardCasePairs) ->
      let
        guards = map fst guardCasePairs
        starts = [Start e] ++ (map Start guards)
        ends = (map End guards) ++ [End e] --TODO go to failure case?
        innerEdges = concatMap (\(g, c) -> [(End g, Start c), (End c, End e)]) guardCasePairs
        --TODO does flow go from each guard to next, or from whole to each guard
      in return $ innerEdges ++ (zip starts ends)
    (Let e1 e2) -> error "TODO implement cases for let expressions"
    (Case e1 cases) -> return $ 
      [(Start e, Start e1)] ++
      concatMap (\(_pat, e2) -> [(End e1, Start e2), (End e2, Start e)]) cases
    (Data ctor args) -> let
        startList = [Start e] ++ map Start args
        endList = (map End args) ++ [End e]
      in return $ zip startList endList
    (Access e1 _) -> return $ [(Start e, Start e1), (End e1, End e)]
    (Remove e1 _) -> return $ [(Start e, Start e1), (End e1, End e)]
    (Insert e1 _ e2) -> return $ [(Start e, Start e1), (End e1, Start e2), (End e2, End e)]
    (Modify e1 newVals) ->
      let
        exprs = map snd newVals
        starts = [Start e] ++ map Start exprs
        ends = (map End exprs) ++ [End e]
        
      in return $ zip starts ends
    (Record nameExprs) ->
      let
        exprs = map snd nameExprs
        starts = [Start e] ++ map Start exprs
        ends = (map End exprs) ++ [End e]
      in return $ zip starts ends
    (PortIn e1 e2) -> return []
    (PortOut _ _ e1) -> return $ [(Start e, Start e1), (End e1, End e)]
    (GLShader _ _ _ ) -> return []



removeDeadCodeWP :: [Name] 
  -> Map.Map Name (Module, Interface)
  -> Map.Map Name (Module, Interface) 
removeDeadCodeWP _ m = m --TODO implement


removeDeadCodeModule :: Name -> (Module, Interface) -> (Module, Interface)
removeDeadCodeModule n (m,i) =
  let
    newMod = tformModule removeDeadCode m
  in (newMod,i)

removeDeadCode :: Canon.Expr -> Canon.Expr
removeDeadCode e =
  let
    eAnn = annotateCanonical  (Label []) e
  in e --TODO implement
