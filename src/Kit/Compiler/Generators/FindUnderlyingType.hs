module Kit.Compiler.Generators.FindUnderlyingType where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Generators.NameMangling
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
findUnderlyingType
  :: CompileContext -> Module -> Maybe Span -> ConcreteType -> IO BasicType
findUnderlyingType ctx mod pos t = do
  x <- case t of
    TypeBasicType b       -> return b
    TypeAtom              -> return $ BasicTypeAtom
    TypeAnonStruct fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod pos t
          return (name, t')
        )
      return $ BasicTypeStruct Nothing fields'
    TypeAnonUnion fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod pos t
          return (name, t')
        )
      return $ BasicTypeUnion Nothing fields'
    TypeInstance (modPath, name) p -> do
      templateDef <- getTypeDefinition ctx modPath name
      mod         <- getMod ctx modPath
      modTctx     <- modTypeContext ctx mod
      params      <- forM p (mapType $ follow ctx modTctx)
      let tctx = addTypeParams
            modTctx
            [ (paramName param, value)
            | (param, value) <- zip (typeParams templateDef) params
            ]
      def <- followType ctx tctx templateDef
      let typeName = if null params then name else monomorphName name params
      case typeSubtype def of
        Struct { structFields = fields } -> do
          fields <- forM fields $ \field -> do
            t <- findUnderlyingType ctx mod pos (varType field)
            return (varName field, t)
          return $ BasicTypeStruct (Just typeName) fields
        Union { unionFields = fields } -> do
          fields <- forM fields $ \field -> do
            t <- findUnderlyingType ctx mod pos (varType field)
            return (varName field, t)
          return $ BasicTypeUnion (Just typeName) fields
        enum@(Enum { enumVariants = variants }) -> do
          if enumIsSimple enum
            then return $ BasicTypeSimpleEnum (Just typeName) $ map
              variantName
              variants
            else do
              variants' <- forM variants $ \variant -> do
                args <- forM (variantArgs variant) $ \arg -> do
                  t <- findUnderlyingType ctx mod pos $ argType arg
                  return (argName arg, t)
                return (variantName variant, args)
              return $ BasicTypeComplexEnum typeName variants'
        Abstract { abstractUnderlyingType = u } ->
          findUnderlyingType ctx mod pos u

            -- typeDef       <- h_lookup (modContents definitionMod) name
      -- TODO
    -- TypeTypedef TypePath [ConcreteType]
    -- TypeFunction ConcreteType ConcreteArgs Bool
    -- TypePtr ConcreteType
    -- TypeArr ConcreteType (Maybe Int)
    -- TypeEnumConstructor TypePath ConcreteArgs
    -- TypeRange
    -- TypeTraitPointer TypePath
    TypePtr t -> do
      t' <- findUnderlyingType ctx mod pos t
      return $ CPtr t'
    TypeTypeVar tv -> do
      tctx  <- newTypeContext [] -- TODO...
      known <- follow ctx tctx t
      case known of
        TypeTypeVar id -> do
          info <- getTypeVar ctx id
          throwk $ BasicError
            ("The type of this expression is ambiguous; not enough information to infer a type for type var #"
            ++ show id
            ++ ".\n\nTry adding a type annotation: `expression: Type`"
            )
            (Just $ head $ typeVarPositions info)
        _ -> findUnderlyingType ctx mod pos known
    TypeTuple t -> do
      slots <- forM t (findUnderlyingType ctx mod pos)
      return $ BasicTypeTuple
        (s_pack (basicTypeAbbreviation $ BasicTypeTuple "" slots))
        slots
    TypeFunction rt args var params -> do
      rt'   <- findUnderlyingType ctx mod pos rt
      args' <- forM
        args
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod pos t
          return (name, t')
        )
      return $ BasicTypeFunction rt' args' var
    TypeBox (modPath, name) params -> do
      let name' = (mangleName (modPath ++ [name]) "box")
      return $ BasicTypeStruct (Just name') []
    TypeTypeParam t -> do
      throwk $ InternalError
        (  "Couldn't find underlying type for type param "
        ++ s_unpack t
        ++ "; this is probably an error with monomorph generation!"
        )
        pos
    _ -> do
      -- TODO: REMOVE
      throwk
        $ InternalError ("Couldn't find underlying type for " ++ show t) pos

  case x of
    BasicTypeTuple name t -> h_insert (modTuples mod) (s_unpack name) x
    _                     -> return ()

  return x
