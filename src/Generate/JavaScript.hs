module Generate.JavaScript (generate) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first,(***))
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax


import AST.Module hiding (body)
import AST.Expression.General
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Helpers as Help
import AST.Literal
import qualified AST.Module as Module
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import Generate.JavaScript.Helpers as Help
import qualified Generate.Cases as Case
import qualified Generate.JavaScript.Port as Port
import qualified Generate.JavaScript.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


exprStmt :: Expression () -> Statement ()
exprStmt e =
  case e of
    CallExpr () (FuncExpr () _ [] stmts) [] ->
      BlockStmt () stmts

    _ ->
      ExprStmt () e

    
internalImports :: Module.Name -> [VarDecl ()]
internalImports name =
    [ varDecl "_N" (obj ["Elm","Native"])
    , include "_U" "Utils"
    , include "_L" "List"
    , varDecl Help.localModuleName (string (Module.nameToString name))
    ]
  where
    include :: String -> String -> VarDecl ()
    include alias modul =
        varDecl alias (Help.make ["_N", modul])


_Utils :: String -> Expression ()
_Utils x =
    obj ["_U", x]


_List :: String -> Expression ()
_List x =
    obj ["_L", x]


literal :: Literal -> Expression ()
literal lit =
  case lit of
    Chr c -> _Utils "chr" <| string [c]
    Str s -> string s
    IntNum   n -> IntLit () n
    FloatNum n -> NumLit () n
    Boolean  b -> BoolLit () b


--Given an expression, generate the statement returning the value of that expression
--Just generates a JS return statement, except in the case that the expression
--is a tail call, in which case it generates argument assignments with a break statement
returnStatement :: Canonical.Expr -> State Int [Statement ()]
returnStatement expr@(A.A ann e) =
  --Special case: if we return from a tail call, we don't actually return
  --We instead set paramters then break
  case (A.isTailCallWithArgs ann, e) of
    (Just (fnName, argTforms), App e1 e2) ->
      do let getArgs func args =
                case func of
                  (A.A _ (App f arg)) -> getArgs f (arg : args)
                  _ -> (func, args)
             (_, args) = getArgs e1 [e2]
         args' <- mapM expression args
         let tempIds = map (\i -> "_Temp" ++ (show i) ) [0 .. length args']
         
             firstAssignPairs = zip tempIds args'
             
             secondAssignPairs = zip argTforms (map ref tempIds)
             
             makeAssign (patTform, val) =
                  map (\ (argName, tformedVal) ->
                        exprStmt $  AssignExpr () OpAssign (LVar () argName) tformedVal )
                    (patTform val)
                                                         --
             makeInit :: (String, Expression ()) ->
                        Statement ()
             makeInit (arg, val) =
                  VarDeclStmt () [VarDecl () (Id () arg) (Just $ val)]
         return
              $ (map makeInit firstAssignPairs)
                ++ (concatMap makeAssign secondAssignPairs)
                ++ [ContinueStmt () (Just $ Id () fnName)]
              
    --Normal case: just return the expression
    _ ->
      do jsExp <- expression expr
         return $ [ret jsExp]
    

expression :: Canonical.Expr -> State Int (Expression ())
expression (A.A ann expr) =
    case expr of
      Var var ->
          return $ Var.canonical var

      Literal lit ->
          return $ literal lit

      Range lo hi ->
          do  lo' <- expression lo
              hi' <- expression hi
              return $ _List "range" `call` [lo',hi']

      Access e field ->
          do  e' <- expression e
              return $ DotRef () e' (var (Var.varName field))

      Remove e field ->
          do  e' <- expression e
              return $ _Utils "remove" `call` [ string (Var.varName field), e' ]

      Insert e field value ->
          do  value' <- expression value
              e' <- expression e
              return $ _Utils "insert" `call` [ string (Var.varName field), value', e' ]

      Modify e fields ->
          do  e' <- expression e
              fields' <-
                forM fields $ \(field, value) ->
                  do  value' <- expression value
                      return $ ArrayLit () [ string (Var.varName field), value' ]

              return $ _Utils "replace" `call` [ArrayLit () fields', e']

      Record fields ->
          do  fields' <-
                forM fields $ \(field, e) ->
                    (,) (Var.varName field) <$> expression e

              let fieldMap =
                    List.foldl' combine Map.empty fields'

              return $ ObjectLit () $ (prop "_", hidden fieldMap) : visible fieldMap
          where
            combine record (field, value) =
                Map.insertWith (++) field [value] record

            hidden fs =
                ObjectLit () . map (prop *** ArrayLit ()) $
                  Map.toList (Map.filter (not . null) (Map.map tail fs))

            visible fs =
                map (first prop) (Map.toList (Map.map head fs))

      Binop op e1 e2 ->
          binop ann op e1 e2

      --For lambdas, we see if the expression contains tail calls
      --If it does, then we wrap the function body in a while loop and a block statement
      --so that tail calls can just assign arguments and break from the loop
      Lambda pattern rawBody -> 
          do let tcoBodyLoop name body =
                   LabelledStmt () (Id () name) $
                   WhileStmt () (BoolLit () True) $ BlockStmt () [
                     exprStmt body
                     ]
                 tcoFnBody name args body =
                   FuncExpr () Nothing (map var args) [ tcoBodyLoop name body ]
             (args, body) <-
                foldM depattern ([], innerBody) (reverse patterns)
             body' <-
                expression body
             let baseFn =
                    case(A.hasTailCall ann) of
                      Just fnName ->
                        tcoFnBody fnName args body'
                        
                      _ -> (args ==> body')
                      
             return $
                case (length args < 2, length args > 9) of
                  
                  (True, _) ->
                    baseFn
                  
                  --We don't optimize for tail-calls if there's more than 9 arguments
                  (_, True)  ->
                    foldr (==>) body' (map (:[]) args)
                  
                  (False, False) ->
                    ref ("F" ++ show (length args)) <| baseFn
          where 
            depattern (args, body) pattern =
                case pattern of
                  A.A _ (P.Var x) ->
                      return (args ++ [ Var.varName x ], body)
                  _ ->
                      do  arg <- Case.newVar
                          return
                            ( args ++ [arg]
                            , A.A ann (Case (A.A ann (localVar arg)) [(pattern, body)])
                            )

            (patterns, innerBody) =
                collect [pattern] rawBody

            collect patterns lexpr@(A.A _ expr) =
                case expr of
                  Lambda p e -> collect (p:patterns) e
                  _ -> (patterns, lexpr)

      App e1 e2 -> do
        args' <- mapM expression args
        case A.isTailCallWithArgs ann of
          Just _argNames ->
            error "Should have removed tail call earlier"

          Nothing ->
            do func' <- expression func
               return $
                 case args' of
                   [arg] -> func' <| arg
                   _ | length args' <= 9 -> ref aN `call` (func':args')
                     | otherwise         -> foldl1 (<|) (func':args')
          where
            aN = "A" ++ show (length args)
            (func, args) = getArgs e1 [e2]
            getArgs func args =
                case func of
                  (A.A _ (App f arg)) -> getArgs f (arg : args)
                  _ -> (func, args)

      Let defs e ->
          do  let (defs',e') = flattenLets defs e
              stmts <- concat <$> mapM definition defs'
              rsList <- returnStatement e'
              return $ function [] (stmts ++ rsList) `call` []

      MultiIf branches ->
          do  let hasTC (A.A theAnn _) =
                    (Maybe.isJust $ A.isTailCallWithArgs theAnn)
                    || (Maybe.isJust $ A.hasTailCall theAnn)
                  tcValues =
                    map hasTC $ map snd branches
              case (List.or tcValues) of
                --If no tail call: we can translate into conditional expressions in JS
                False ->
                  do branches' <-
                       forM branches $ \(b,e) -> (,) <$> expression b <*> expression e
                     return $
                       case last branches of
                         (A.A _ (Var (Var.Canonical (Var.Module ["Basics"]) "otherwise")), _) ->
                           safeIfs branches'
                           
                         (A.A _ (Literal (Boolean True)), _) ->
                           safeIfs branches'
                           
                         _ ->
                           ifs branches' (throw "badIf" (A.region ann))
                           
                _ -> do
                  --If there is a tail call, then we need to manipulate control flow
                  --So we translate into if statements in JS
                  branches' <-
                    forM branches $
                      \(b,e) -> (,) <$> expression b
                               <*> ( (BlockStmt ()) <$> returnStatement e)
                  let
                    safeIfStmt branchList = ifStmts (init branchList) (snd (last branchList))
                    ifStmts branchList finally = foldr iffStmt finally branchList
                    iffStmt (if', then') else' = IfStmt () if' then' else'
                  let retStmt =
                        case last branches of
                          (A.A _ (Var (Var.Canonical (Var.Module ["Basics"]) "otherwise")), _) ->
                            safeIfStmt branches'
                            
                          (A.A _ (Literal (Boolean True)), _) ->
                            safeIfStmt branches'
                            
                          _ ->
                            ifStmts branches' (exprStmt $ throw "badIf" (A.region ann))
                  return $ function [] [retStmt] `call` []                            
          where
            safeIfs branchList = ifs (init branchList) (snd (last branchList))
            ifs branchList finally = foldr iff finally branchList
            iff (if', then') else' = CondExpr () if' then' else'

      Case e cases ->
          do  (tempVar,initialMatch) <- Case.toMatch cases
              (revisedMatch, stmt) <-
                  case e of
                    A.A _ (Var (Var.Canonical Var.Local x)) ->
                        return (Case.matchSubst [(tempVar, Var.varName x)] initialMatch, [])
                    _ ->
                        do  e' <- expression e
                            return (initialMatch, [VarDeclStmt () [varDecl tempVar e']])
              match' <- match (A.region ann) revisedMatch
              return (function [] (stmt ++ match') `call` [])

      ExplicitList es ->
          do  es' <- mapM expression es
              return $ _List "fromArray" <| ArrayLit () es'

      Data name es ->
          do  es' <- mapM expression es
              return $ ObjectLit () (ctor : fields es')
          where
            ctor = (prop "ctor", string name)
            fields =
                zipWith (\n e -> (prop ("_" ++ show n), e)) [ 0 :: Int .. ]

      GLShader _uid src _tipe ->
          return $ ObjectLit () [(PropString () "src", literal (Str src))]

      Port impl ->
          case impl of
            In name portType ->
                return (Port.inbound name portType)

            Out name expr portType ->
                do  expr' <- expression expr
                    return (Port.outbound name expr' portType)

            Task name expr portType ->
                do  expr' <- expression expr
                    return (Port.task name expr' portType)


definition :: Canonical.Def -> State Int [Statement ()]
definition (Canonical.Definition annPattern expr@(A.A ann _) _) =
  do  expr' <- expression expr
      let assign x = varDecl x expr'
      let (A.A patternAnnot pattern) = annPattern
      case pattern of
        P.Var x
            | Help.isOp x ->
                let op = LBracket () (ref "_op") (string x) in
                return [ exprStmt $ AssignExpr () OpAssign op expr' ]

            | otherwise ->
                return [ VarDeclStmt () [ assign (Var.varName x) ] ]

        P.Record fields ->
            let setField f = varDecl f (obj ["$",f]) in
            return [ VarDeclStmt () (assign "$" : map setField fields) ]

        P.Data (Var.Canonical _ name) patterns | vars /= Nothing ->
            return [ VarDeclStmt () (setup (zipWith decl (maybe [] id vars) [0..])) ]
          where
            vars = getVars patterns
            getVars patterns =
                case patterns of
                  A.A _ (P.Var x) : rest ->
                      (Var.varName x :) `fmap` getVars rest
                  [] ->
                      Just []
                  _ ->
                      Nothing

            decl x n = varDecl x (obj ["$","_" ++ show n])
            setup vars
                | Help.isTuple name = assign "$" : vars
                | otherwise = assign "_raw" : safeAssign : vars

            safeAssign = varDecl "$" (CondExpr () if' (ref "_raw") exception)
            if' = InfixExpr () OpStrictEq (obj ["_raw","ctor"]) (string name)
            exception = Help.throw "badCase" (A.region ann)

        _ ->
            do  defs' <- concat <$> mapM toDef vars
                return (VarDeclStmt () [assign "_"] : defs')
            where
              vars = P.boundVarList annPattern
              mkVar = A.A ann . localVar
              toDef :: String -> State Int [Statement ()]
              toDef y =  
                let
                    expr = A.A ann $ Case (mkVar "_") [(annPattern, mkVar y)]
                    pat = A.A patternAnnot (P.Var y)
                in
                    definition (Canonical.Definition pat expr Nothing)


match :: R.Region -> Case.Match -> State Int [Statement ()]
match region mtch =
  case mtch of
    Case.Match name clauses mtch' ->
        do  (isChars, clauses') <- unzip <$> mapM (clause region name) clauses
            mtch'' <- match region mtch'
            return (SwitchStmt () (format isChars (access name)) clauses' : mtch'')
        where
          isLiteral p =
            case p of
              Case.Clause (Right _) _ _ -> True
              _ -> False

          access name
              | any isLiteral clauses = obj [name]
              | otherwise = obj (Help.splitDots name ++ ["ctor"])

          format isChars e
              | or isChars = InfixExpr () OpAdd e (string "")
              | otherwise = e

    Case.Fail ->
        return [ exprStmt (Help.throw "badCase" region) ]

    Case.Break ->
        return [BreakStmt () Nothing]

    Case.Other e ->
        do  rsList <- returnStatement e
            return rsList

    Case.Seq ms ->
        concat <$> mapM (match region) (dropEnd [] ms)
      where
        dropEnd acc [] = acc
        dropEnd acc (m:ms) =
            case m of
              Case.Other _ -> acc ++ [m]
              _ -> dropEnd (acc ++ [m]) ms


clause :: R.Region -> String -> Case.Clause -> State Int (Bool, CaseClause ())
clause region variable (Case.Clause value vars mtch) =
    (,) isChar . CaseClause () pattern <$> match region (Case.matchSubst (zip vars vars') mtch)
  where
    vars' =
        map (\n -> variable ++ "._" ++ show n) [0..]

    (isChar, pattern) =
        case value of
          Right (Chr c) -> (True, string [c])
          _ ->
            (,) False $
              case value of
                Right (Boolean b) -> BoolLit () b
                Right lit -> literal lit
                Left (Var.Canonical _ name) ->
                    string name


flattenLets :: [Canonical.Def] -> Canonical.Expr -> ([Canonical.Def], Canonical.Expr)
flattenLets defs lexpr@(A.A _ expr) =
    case expr of
      Let ds body -> flattenLets (defs ++ ds) body
      _ -> (defs, lexpr)


generate :: Module.CanonicalModule -> String
generate modul =
    show . prettyPrint $ setup "Elm" (names ++ ["make"]) ++
             [ assign ("Elm" : names ++ ["make"]) (function [localRuntime] programStmts) ]
  where
    names :: [String]
    names = Module.names modul

    thisModule :: Expression ()
    thisModule = obj (localRuntime : names ++ ["values"])

    programStmts :: [Statement ()]
    programStmts =
        concat
        [ [ exprStmt (string "use strict") ]
        , setup localRuntime (names ++ ["values"])
        , [ IfSingleStmt () thisModule (ret thisModule) ]
        , [ VarDeclStmt () localVars ]
        , body
        , [ jsExports ]
        , [ ret thisModule ]
        ]

    localVars :: [VarDecl ()]
    localVars =
        varDecl "_op" (ObjectLit () [])
        : internalImports (Module.names modul)
        ++ explicitImports
      where
        explicitImports :: [VarDecl ()]
        explicitImports =
            Module.imports modul
              |> Set.fromList
              |> Set.toList
              |> map jsImport

        jsImport :: Module.Name -> VarDecl ()
        jsImport name =
            varDecl (Var.moduleName name) $
                obj ("Elm" : name ++ ["make"]) <| ref localRuntime

    body :: [Statement ()]
    body =
        concat (evalState defs 0)
      where
        defs =
            Module.program (Module.body modul)
              |> flattenLets []
              |> fst
              |> mapM definition

    setup namespace path =
        map create paths
      where
        create name =
            assign name (InfixExpr () OpLOr (obj name) (ObjectLit () []))
        paths =
            namespace : path
              |> List.inits
              |> init
              |> drop 2

    jsExports =
        assign (localRuntime : names ++ ["values"]) (ObjectLit () exs)
      where
        exs = map entry $ "_op" : concatMap extract (exports modul)
        entry x = (prop x, ref x)
        extract value =
            case value of
              Var.Alias _ -> []

              Var.Value x
                | Help.isOp x -> []
                | otherwise   -> [Var.varName x]

              Var.Union _ (Var.Listing ctors _) ->
                  map Var.varName ctors

    assign path expr =
      case path of
        [x] -> VarDeclStmt () [ varDecl x expr ]
        _ ->
          exprStmt $
          AssignExpr () OpAssign (LDot () (obj (init path)) (last path)) expr


binop
    :: A.ExprMetaData
    -> Var.Canonical
    -> Canonical.Expr
    -> Canonical.Expr
    -> State Int (Expression ())
binop ann func@(Var.Canonical home op) e1 e2 =
    case (home, op) of
      (Var.Module ["Basics"], ">>") ->
        do  es <- mapM expression (collectLeftAssoc [e2] e1)
            return $ ["$"] ==> List.foldl' (\e f -> f <| e) (ref "$") es

      (Var.Module ["Basics"], "<<") ->
        do  es <- mapM expression (e1 : collectRightAssoc [] e2)
            return $ ["$"] ==> foldr (<|) (ref "$") es

      (Var.Module ["Basics"], "<|") ->
        do  e2' <- expression e2
            es <- mapM expression (collectRightAssoc [] e1)
            return $ foldr (<|) e2' es

      (Var.BuiltIn, "::") ->
        expression (A.A ann (Data "::" [e1,e2]))

      (Var.Module ["Basics"], _) ->
        do  e1' <- expression e1
            e2' <- expression e2
            case Map.lookup op basicOps of
              Just f ->
                  return (f e1' e2')

              Nothing ->
                  return (ref "A2" `call` [ Var.canonical func, e1', e2' ])

      _ ->
        do  e1' <- expression e1
            e2' <- expression e2
            return (ref "A2" `call` [ Var.canonical func, e1', e2' ])

  where
    collectRightAssoc es e =
        case e of
          A.A _ (Binop (Var.Canonical (Var.Module ["Basics"]) "<<") e1 e2) ->
              collectRightAssoc (es ++ [e1]) e2
          _ -> es ++ [e]

    collectLeftAssoc es e =
        case e of
          A.A _ (Binop (Var.Canonical (Var.Module ["Basics"]) ">>") e1 e2) ->
              collectLeftAssoc (e2 : es) e1
          _ -> e : es

    basicOps =
        Map.fromList (infixOps ++ specialOps)

    infixOps =
        let infixOp str op = (str, InfixExpr () op) in
        [ infixOp "+"  OpAdd
        , infixOp "-"  OpSub
        , infixOp "*"  OpMul
        , infixOp "/"  OpDiv
        , infixOp "&&" OpLAnd
        , infixOp "||" OpLOr
        ]

    specialOps =
        [ (,) "^"  $ \a b -> obj ["Math","pow"] `call` [a,b]
        , (,) "|>" $ flip (<|)
        , (,) "==" $ \a b -> _Utils "eq" `call` [a,b]
        , (,) "/=" $ \a b -> PrefixExpr () PrefixLNot (_Utils "eq" `call` [a,b])
        , (,) "<"  $ cmp OpLT 0
        , (,) ">"  $ cmp OpGT 0
        , (,) "<=" $ cmp OpLT 1
        , (,) ">=" $ cmp OpGT (-1)
        , (,) "//" $ \a b -> InfixExpr () OpBOr (InfixExpr () OpDiv a b) (IntLit () 0)
        ]

    cmp op n a b =
        InfixExpr () op (_Utils "cmp" `call` [a,b]) (IntLit () n)
