module Kit.Compiler.Generators.TypeSpecialize where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

{-
  Recursively dereference a high level ConcreteType into a BasicType.
-}
findUnderlyingType :: CompileContext -> Module -> ConcreteType -> IO BasicType
findUnderlyingType ctx mod t = do
  x <- case t of
    TypeBasicType b       -> return b
    TypeAtom              -> return $ BasicTypeAtom
    TypeAnonStruct fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod t
          return (name, t')
        )
      return $ BasicTypeStruct Nothing fields'
    TypeAnonUnion fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod t
          return (name, t')
        )
      return $ BasicTypeUnion Nothing fields'
    TypeInstance (modPath, name) params -> do
      def <- getTypeDefinition ctx modPath name
      case def of
        Just (TypeDefinition { typeSubtype = subtype }) -> case subtype of
          Struct { structFields = fields } -> do
            fields <- forM fields $ \field -> do
              t <- findUnderlyingType ctx mod (varType field)
              return (varName field, t)
            return $ BasicTypeStruct (Just name) fields
          Union { unionFields = fields } -> do
            fields <- forM fields $ \field -> do
              t <- findUnderlyingType ctx mod (varType field)
              return (varName field, t)
            return $ BasicTypeUnion (Just name) fields
          enum@(Enum { enumVariants = variants }) -> do
            if enumIsSimple enum
              then return $ BasicTypeSimpleEnum (Just name) $ map variantName
                                                                  variants
              else do
                variants' <- forM variants $ \variant -> do
                  args <- forM (variantArgs variant) $ \arg -> do
                    t <- findUnderlyingType ctx mod $ argType arg
                    return (argName arg, t)
                  return (variantName variant, args)
                return $ BasicTypeComplexEnum name variants'
          Abstract { abstractUnderlyingType = u } ->
            findUnderlyingType ctx mod u
        _ -> throwk $ BasicError
          (  "Unexpected missing type definition: "
          ++ (s_unpack $ showTypePath (modPath, name))
          )
          Nothing
      -- typeDef       <- h_lookup (modContents definitionMod) name
      -- TODO
    -- TypeTypedef TypePath [ConcreteType]
    -- TypeFunction ConcreteType ConcreteArgs Bool
    -- TypePtr ConcreteType
    -- TypeArr ConcreteType (Maybe Int)
    -- TypeEnumConstructor TypePath ConcreteArgs
    -- TypeIdentifier ConcreteType
    -- TypeRange
    -- TypeTraitPointer TypePath
    TypePtr t -> do
      t' <- findUnderlyingType ctx mod t
      return $ CPtr t'
    TypeTypeVar tv -> do
      tctx  <- newTypeContext [] -- TODO...
      known <- knownType ctx tctx mod t
      case known of
        TypeTypeVar id -> if tv == id
          then findDefaultType ctx mod id
          else findUnderlyingType ctx mod known
        _ -> findUnderlyingType ctx mod known
    TypeTuple t -> do
      slots <- forM t (findUnderlyingType ctx mod)
      return $ BasicTypeTuple
        (s_pack (basicTypeAbbreviation $ BasicTypeTuple "" slots))
        slots
    TypeFunction rt args var -> do
      rt'   <- findUnderlyingType ctx mod rt
      args' <- forM
        args
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod t
          return (name, t')
        )
      return $ BasicTypeFunction rt' args' var
    TypeBox (modPath, name) params -> do
      let name' = (mangleName (modPath ++ [name]) "box")
      return $ BasicTypeStruct (Just name') []
    _ -> do
      -- TODO: REMOVE
      throwk $ InternalError ("Couldn't find underlying type for " ++ show t)
                             Nothing

  case x of
    BasicTypeTuple name t -> h_insert (modTuples mod) (s_unpack name) x
    _                     -> return ()

  return x

-- TODO: finding specializations should be done as a separate step
findDefaultType :: CompileContext -> Module -> Int -> IO BasicType
findDefaultType ctx mod id = do
  info <- getTypeVar ctx id
  if null (typeVarConstraints info)
    then throwk $ BasicError
      ("The type of this expression is ambiguous; not enough information to infer a type for type var #"
      ++ show id
      ++ ".\n\nTry adding a type annotation: `expression: Type`"
      )
      (Just $ head $ typeVarPositions info)
    else do
      tctx <- newTypeContext []
      let constraints = typeVarConstraints info
      defaults <- mapM (h_lookup (ctxTraitSpecializations ctx))
                       (map (fst . fst) constraints)
      let specializations = catMaybes defaults
      specialization <- foldM
        (\acc (tp, _) -> do
          spec <- resolveType ctx tctx mod tp
          case acc of
            Just _  -> return acc
            Nothing -> do
              meetConstraints <- foldM
                (\acc' c -> case acc' of
                  Just _ -> do
                    -- FIXME: params
                    result <- unify ctx
                                    tctx
                                    mod
                                    spec
                                    (TypeTraitConstraint (c, []))
                    return $ case result of
                      Just _ -> acc'
                      _      -> Nothing
                  Nothing -> do
                    return acc'
                )
                (Just spec)
                (map (fst . fst) constraints)
              case meetConstraints of
                Just _  -> return meetConstraints
                Nothing -> return Nothing
        )
        Nothing
        specializations
      case specialization of
        Just t -> do
          tctx <- newTypeContext []
          findUnderlyingType ctx mod t
        _ -> throwk $ BasicError
          ("This expression has constraints: \n\n"
          ++ (intercalate
               "\n"
               [ "  - " ++ s_unpack (showTypePath c) ++ " (" ++ reason ++ ")"
               | ((c, _), (reason, _)) <- constraints
               ]
             )
          ++ "\n\nbut no specialization for one of these traits satisfies all of them, so no concrete type can be determined.\n\nTry adding a type annotation: `(myExpression: Type)`"
          )
          (Just $ head $ typeVarPositions info)
