module Kit.Compiler.Ir.FindUnderlyingType where

import Control.Monad
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
import Kit.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

{-
  Recursively dereference a high level ConcreteType into a BasicType.
-}
findUnderlyingType
  :: CompileContext -> Module -> Maybe Span -> ConcreteType -> IO BasicType
findUnderlyingType ctx mod pos t = _findUnderlyingType ctx mod pos [] t
_findUnderlyingType ctx mod pos stack t = do
  let r x = _findUnderlyingType ctx mod pos (x : stack) x
  when (length stack > 256) $ throwk $ InternalError
    ("Maximum recursion depth in findUnderlyingType exceeded; " ++ show stack)
    Nothing
  veryNoisyDebugLog ctx $ "find underlying type " ++ show t
  modTctx <- modTypeContext ctx mod
  x       <- case t of
    TypeBasicType b       -> return b
    TypeAtom              -> return $ BasicTypeAtom
    TypeAnonStruct fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- r t
          return (name, t')
        )
      return $ BasicTypeAnonStruct fields'
    TypeAnonUnion fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- r t
          return (name, t')
        )
      return $ BasicTypeAnonUnion fields'
    TypeAnonEnum variants ->
      return $ BasicTypeAnonEnum [ ([], n) | n <- variants ]

    -- TypeTypedef TypePath [ConcreteType]
    -- TypeFunction ConcreteType ConcreteArgs Bool
    -- TypePtr ConcreteType
    -- TypeEnumConstructor TypePath ConcreteArgs
    -- TypeRange
    -- TypeTraitPointer TypePath
    TypeArray t s -> do
      t <- r t
      return $ CArray t (if s == 0 then Nothing else Just s)
    TypeInstance (["kit", "common"], "CArray") [t, s] -> do
      tctx <- newTypeContext []
      size <- follow ctx tctx s
      case size of
        s@(ConstantType (IntValue i)) -> findUnderlyingType ctx mod pos (TypeArray t i)
        _                  -> throwk
          $ BasicError ("Invalid Array size parameter: " ++ show size) pos
    TypePtr t -> do
      t' <- r t
      return $ CPtr t'
    TypeTypeVar tv -> do
      tctx  <- newTypeContext []
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
        _ -> r known
    TypeTuple t -> do
      slots <- forM t (r)
      return $ BasicTypeTuple (tupleName slots) slots
    TypeFunction rt args var params -> do
      rt'   <- r rt
      args' <- forM
        args
        (\(name, t) -> do
          t' <- r t
          return (name, t')
        )
      return $ BasicTypeFunction rt' args' var
    TypeBox tp params -> do
      params <- forM params $ mapType $ follow ctx modTctx
      return $ BasicTypeStruct $ subPath (monomorphName tp params) "box"
    TypeTypeParam t -> do
      throwk $ InternalError
        (  "Couldn't find underlying type for type param "
        ++ s_unpack (showTypePath t)
        ++ "; this is probably an error with monomorph generation!"
        )
        pos
    ConstantType x    -> return BasicTypeUnknown {-throwk $ InternalError
      ("Constant type (" ++ show x ++ ") can't be used as the type of a value")
      pos-}
    TypeInstance tp p -> do
      templateDef <- getTypeDefinition ctx tp
      params      <- forM p (mapType $ follow ctx modTctx)
      let tctx = addTypeParams
            modTctx
            [ (typeSubPath templateDef $ paramName param, value)
            | (param, value) <- zip (typeParams templateDef) params
            ]
      def <- followType ctx tctx templateDef
      let typeName = monomorphName (typeRealName templateDef) params
      case typeSubtype def of
        Struct { structFields = fields } -> do
          return $ BasicTypeStruct typeName
        Union { unionFields = fields } -> do
          return $ BasicTypeUnion typeName
        enum@(Enum { enumVariants = variants }) -> do
          return $ if enumIsSimple enum
            then BasicTypeSimpleEnum typeName
            else BasicTypeComplexEnum typeName
        Abstract { abstractUnderlyingType = u } -> r u
    _ -> -- TODO: REMOVE
         throwk
      $ InternalError ("Couldn't find underlying type for " ++ show t) pos

  case x of
    BasicTypeTuple name t -> h_insert (modTuples mod) (s_unpack name) x
    _                     -> return ()

  return x
