module Kit.Compiler.Typers.TypeExpression.TypeCall
  ( typeCall
  , findImplicit
  )
where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Kit.Ast
import           Kit.Compiler.Binding
import           Kit.Compiler.Context
import           Kit.Compiler.Module
import           Kit.Compiler.TypeContext
import           Kit.Compiler.Typers.AutoRefDeref
import           Kit.Compiler.Typers.ExprTyper
import           Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import           Kit.Compiler.Unify
import           Kit.Compiler.Utils
import           Kit.Error
import           Kit.Str

typeCall :: SubTyper
typeCall (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (Call e1 _ args) -> do
      tryRewrite (makeExprTyped (Call e1 [] args) (inferredType ex) pos) $ do
        (r1 : typedArgs) <- mapM r $ e1 : args
        modImp           <- modImplicits mod
        let untypedImplicits = tImplicits r1 ++ tctxImplicits tctx ++ modImp
        implicits <- forM untypedImplicits $ \i -> do
          t <- follow ctx tctx $ inferredType i
          return $ i { inferredType = t }
        tryRewrite (makeExprTyped (Call r1 [] typedArgs) (inferredType ex) pos)
          $ do
              let
                fail t = throwk
                  $ TypingError ("Type " ++ show t ++ " is not callable") pos
              let
                typeCall t = case t of
                  TypePtr t@(TypeFunction _ _ _ _) ->
                    -- function pointer call
                                                      typeFunctionCall
                    ctx
                    tctx
                    mod
                    (r1 { inferredType = t })
                    implicits
                    typedArgs
                  TypeFunction _ _ _ _ -> typeFunctionCall
                    ctx
                    tctx
                    mod
                    (r1 { inferredType = t })
                    implicits
                    typedArgs
                  TypeEnumConstructor tp discriminant argTypes params -> do
                    typeEnumConstructorCall ctx
                                            tctx
                                            mod
                                            (r1 { inferredType = t })
                                            typedArgs
                                            tp
                                            discriminant
                                            argTypes
                                            params
                  TypeInstance tp params -> do
                    templateDef <- getTypeDefinition ctx tp
                    tctx        <- addTypeParams
                      ctx
                      (tctx { tctxSelf = Just t })
                      [ (typeSubPath templateDef $ paramName param, val)
                      | (param, val) <- zip (typeParams templateDef) params
                      ]
                      pos

                    def <- followType ctx tctx templateDef
                    let subtype = typeSubtype def
                    case subtype of
                      Abstract { abstractUnderlyingType = u } ->
                        -- forward to parent
                        typeCall u
                      _ -> fail t
                  _ -> fail t

              typeCall $ inferredType r1

    (Return e1) -> do
      r1 <- case e1 of
        Just x -> do
          x <- r x
          return $ Just x
        Nothing -> return Nothing
      case (tctxReturnType tctx, r1) of
        (Just rt, Just r1) -> do
          rt <- follow ctx tctx rt
          r1 <- tryAutoRefDeref ctx tctx rt r1
          resolve $ TypeEq (rt)
                           (inferredType r1)
                           "Return type should match function return type"
                           (tPos r1)
          -- make sure we aren't returning any pointers to the stack
          let
            localPointers = exprFilterMapReduce
              (\x -> case tExpr x of
                PreUnop Deref _ -> False
                Call _ _ _      -> False
                _               -> True
              )
              (\x -> if (isPtr $ inferredType x) && (tIsLocalPtr x)
                then [x]
                else []
              )
              (++)
              tExpr
              []
              r1
          unless (null localPointers) $ throwk $ KitErrors
            [ KitError
              $ TypingError "Can't return a pointer to a local value"
              $ tPos ptr
            | ptr <- localPointers
            ]

          return $ makeExprTyped (Return $ Just r1) TypeVoid pos
        (Just rt, Nothing) -> do
          resolve $ TypeEq TypeVoid
                           (rt)
                           "Empty return is only allowed in Void functions"
                           pos
          return $ makeExprTyped (Return Nothing) TypeVoid pos
        (Nothing, _) ->
          throwk $ TypingError "Can't `return` outside of a function" pos

alignCallArgs
  :: CompileContext
  -> TypeContext
  -> [ConcreteType]
  -> Bool
  -> [TypedExpr]
  -> [TypedExpr]
  -> IO [TypedExpr]
alignCallArgs ctx tctx []       isVariadic implicits args = return []
alignCallArgs ctx tctx argTypes isVariadic implicits args = do
  nextArg <- follow ctx tctx (head argTypes)
  found   <- findImplicit ctx tctx nextArg implicits
  case found of
    Just (x, y) -> do
      rest <- alignCallArgs ctx
                            tctx
                            (tail argTypes)
                            isVariadic
                            (delete x implicits)
                            args
      return $ y : rest
    Nothing -> return $ args

findImplicit
  :: CompileContext
  -> TypeContext
  -> ConcreteType
  -> [TypedExpr]
  -> IO (Maybe (TypedExpr, TypedExpr))
findImplicit ctx tctx ct []      = return Nothing
findImplicit ctx tctx ct (h : t) = do
  converted <- tryAutoRefDeref ctx tctx ct h
  match     <- unifyStrict ctx tctx ct (inferredType converted)
  case match of
    Just info ->
      if or
           [ case i of
               TypeVarIs _ _ -> True
               _             -> False
           | i <- info
           ]
        then findImplicit ctx tctx ct t
        else return $ Just (h, converted)
    Nothing -> findImplicit ctx tctx ct t

argDescriptions :: [ConcreteType] -> [ConcreteType] -> [String]
argDescriptions [] [] = []
argDescriptions [] explicits =
  [ (show $ length explicits)
      ++ " explicit argument"
      ++ plural (length explicits)
      ++ ":\n\n"
      ++ (intercalate "\n" [ "  - " ++ show t | t <- explicits ])
  ]
argDescriptions (MethodTarget t : implicits) explicits =
  ("`this` value:\n\n  - " ++ show t) : (argDescriptions implicits explicits)
argDescriptions implicits explicits =
  (  (show $ length implicits)
    ++ " implicit argument"
    ++ plural (length implicits)
    ++ ":\n\n"
    ++ (intercalate "\n" [ "  - " ++ show t | t <- implicits ])
    )
    : (argDescriptions [] explicits)

addDefaultArgs :: ConcreteArgs -> [TypedExpr] -> [TypedExpr]
addDefaultArgs [] new = reverse new
addDefaultArgs ((ArgSpec { argType = argType, argDefault = Just x }) : t) new =
  addDefaultArgs t ((x { inferredType = argType }) : new)
addDefaultArgs (h : t) new = addDefaultArgs t new

typeFunctionCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> [TypedExpr]
  -> IO TypedExpr
typeFunctionCall ctx tctx mod e@(TypedExpr { inferredType = ft@(TypeFunction rt argSpecs isVariadic params), tPos = pos }) implicits args
  = do
    aligned <- alignCallArgs ctx
                             tctx
                             (map argType argSpecs)
                             (isJust isVariadic)
                             implicits
                             args
    let defaults      = addDefaultArgs (drop (length aligned) argSpecs) []
    let totalArgs     = aligned ++ defaults
    let usedImplicits = length aligned - length args
    when
        (if isJust isVariadic
          then length totalArgs < length argSpecs
          else length totalArgs /= length argSpecs
        )
      $ throwk
      $ TypingError
          (  "Function expected "
          ++ (show $ length argSpecs)
          ++ (if isJust isVariadic then " or more" else "")
          ++ " argument"
          ++ (plural $ length argSpecs)
          ++ ":\n\n  "
          ++ show ft
          ++ "\n\nCalled with "
          ++ let descriptions = argDescriptions
                   (map inferredType $ take usedImplicits aligned)
                   (map inferredType $ drop usedImplicits totalArgs)
             in  if null descriptions
                   then "no arguments"
                   else
                     (intercalate
                       "\n\n"
                       [ (if n > 0 then "and " else "") ++ d
                       | (n, d) <- zip [0 ..] descriptions
                       ]
                     )
          )
          pos
    converted <- forMWithErrors
      (zip (map Just argSpecs ++ repeat Nothing) aligned)
      (\(arg, argValue) -> case arg of
        Just (ArgSpec { argType = argType }) ->
          tryAutoRefDeref ctx tctx argType argValue
        Nothing -> return argValue
      )
    forMWithErrors_
      (zip argSpecs converted)
      (\((ArgSpec { argType = argType }), argValue) -> do
        t1 <- follow ctx tctx argType
        t2 <- follow ctx tctx (inferredType argValue)
        resolveConstraint
          ctx
          tctx
          (TypeEq t1
                  t2
                  "Function arg types must match the function's declaration"
                  (tPos argValue)
          )
      )
    return $ makeExprTyped
      (Call e
            (take usedImplicits converted)
            (drop usedImplicits converted ++ defaults)
      )
      rt
      pos

typeEnumConstructorCall
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypedExpr
  -> [TypedExpr]
  -> TypePath
  -> TypePath
  -> ConcreteArgs
  -> [ConcreteType]
  -> IO TypedExpr
typeEnumConstructorCall ctx tctx mod e args tp discriminant argSpecs params =
  do
    when (length args < length argSpecs) $ throwk $ TypingError
      (  "Expected "
      ++ (show $ length argSpecs)
      ++ " arguments (called with "
      ++ (show $ length args)
      ++ ")"
      )
      (tPos e)
    def  <- getTypeDefinition ctx tp
    tctx <- addTypeParams
      ctx
      tctx
      [ (typeSubPath def $ paramName param, value)
      | (param, value) <- zip (typeParams def) params
      ]
      (tPos e)
    forMWithErrors_
      (zip argSpecs args)
      (\((ArgSpec { argType = argType }), argValue) -> do
        t1 <- follow ctx tctx argType
        t2 <- follow ctx tctx (inferredType argValue)
        resolveConstraint
          ctx
          tctx
          (TypeEq t1
                  t2
                  "Enum arg types must match the enum's declaration"
                  (tPos argValue)
          )
      )
    let ct = TypeInstance tp params
    return $ makeExprTyped
      (EnumInit ct discriminant (zip (map argName argSpecs) args))
      ct
      (tPos e)
