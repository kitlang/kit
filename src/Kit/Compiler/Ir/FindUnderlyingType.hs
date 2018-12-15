module Kit.Compiler.Ir.FindUnderlyingType where

import Control.Monad
import Data.IORef
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

recordTuples ctx mod t = do
  case t of
    TypeTuple _ -> modifyIORef (ctxTuples ctx) $ \x -> (modPath mod, t) : x
    _           -> return ()

{-
  Recursively dereference a high level ConcreteType into a BasicType.
-}
findUnderlyingType
  :: CompileContext -> Module -> Maybe Span -> ConcreteType -> IO BasicType
findUnderlyingType ctx mod pos t = _findUnderlyingType ctx mod pos [] t
_findUnderlyingType ctx mod pos stack t = do
  tctx <- newTypeContext []
  recordTuples ctx mod t
  t <- mapType (follow ctx tctx) t
  let r x = _findUnderlyingType ctx mod pos (x : stack) x
  when (length stack > 256) $ throwk $ InternalError
    ("Maximum recursion depth in findUnderlyingType exceeded; " ++ show stack)
    Nothing
  veryNoisyDebugLog ctx $ "find underlying type " ++ show t
  modTctx <- modTypeContext ctx mod
  case t of
    TypeBasicType b         -> return b
    TypeAnonStruct s fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- r t
          return (name, t')
        )
      return $ BasicTypeAnonStruct s fields'
    TypeAnonUnion s fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- r t
          return (name, t')
        )
      return $ BasicTypeAnonUnion s fields'
    TypeAnonEnum s variants -> return $ BasicTypeAnonEnum s variants

    -- TypeTypedef TypePath [ConcreteType]
    -- TypeFunction ConcreteType ConcreteArgs Bool
    -- TypePtr ConcreteType
    -- TypeEnumConstructor TypePath ConcreteArgs
    -- TypeRange
    -- TypeTraitPointer TypePath
    TypeConst t             -> do
      t <- r t
      return $ BasicTypeConst t
    TypeArray t s -> do
      t <- r t
      return $ CArray t (if s == 0 then Nothing else Just s)
    TypeBool    -> return BasicTypeBool
    TypeChar    -> return BasicTypeCChar
    TypeSize    -> return BasicTypeCSize
    TypeInt   0 -> return BasicTypeCInt
    TypeInt   w -> return $ BasicTypeInt w
    TypeUint  0 -> return BasicTypeCUint
    TypeUint  w -> return $ BasicTypeUint w
    TypeFloat w -> return $ BasicTypeFloat w
    TypeInstance (["kit", "common"], "CArray") [t, s] -> do
      tctx <- newTypeContext []
      size <- follow ctx tctx s
      case size of
        s@(ConstantType (IntValue i)) ->
          findUnderlyingType ctx mod pos (TypeArray t i)
        _ -> throwk
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
      return $ BasicTypeFunction rt' args' $ isJust var
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
    ConstantType x -> return BasicTypeUnknown
    ModuleType tp ->
      throwk $ InternalError "Modules can't be used as runtime values" pos
    TypeInstance tp p -> do
      templateDef <- getTypeDefinition ctx tp
      params      <- forM p (mapType $ follow ctx modTctx)
      tctx        <- addTypeParams
        ctx
        modTctx
        [ (typeSubPath templateDef $ paramName param, value)
        | (param, value) <- zip (typeParams templateDef) params
        ] (fromJust pos)
      def <- followType ctx tctx templateDef
      let typeName = monomorphName (typeRealName templateDef) params
      case typeSubtype def of
        StructUnion { structUnionFields = fields, isStruct = isStruct } -> do
          return
            $ (if isStruct then BasicTypeStruct else BasicTypeUnion) typeName
        enum@(Enum { enumVariants = variants }) -> do
          return $ if enumIsSimple enum
            then BasicTypeSimpleEnum typeName
            else BasicTypeComplexEnum typeName
        Abstract { abstractUnderlyingType = u } -> r u

    TypeTraitConstraint (tp, p) -> do
      return $ BasicTypeStruct $ subPath (monomorphName tp p) "vtable"

    MethodTarget t -> r t

    _              -> -- TODO: REMOVE
                      throwk
      $ InternalError ("Couldn't find underlying type for " ++ show t) pos
