module Kit.Compiler.Typers.TypeExpression.TypeVarBinding (typeVarBinding) where

import Control.Monad
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Unify
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.ExprTyper
import Kit.Error
import Kit.Str

typeVarBinding
  :: CompileContext -> TypeContext -> TypedBinding -> Span -> IO TypedExpr
typeVarBinding ctx tctx binding pos = do
  let invalidBinding = throwk $ InternalError "Invalid binding" (Just pos)
  let returnTypeBinding tp params = return
        $ makeExprTyped (Identifier (Var $ tp)) (TypeTypeOf tp params) pos
  let returnTraitBinding tp params = return $ makeExprTyped
        (Identifier (Var tp))
        (TypeTraitConstraint (tp, params))
        pos
  case binding of
    ExprBinding     x   -> return x
    EnumConstructor def -> do
      let parentTp     = variantParent def
      let discriminant = variantName def
      let extern       = hasMeta "extern" (variantMeta def)
      -- FIXME: pull params from tctx
      params <- makeGeneric ctx parentTp pos []
      let ct   = TypeInstance parentTp $ map snd params
      let args = [ (argName arg, argType arg) | arg <- variantArgs def ]
      if null args
        then return $ makeExprTyped (EnumInit ct discriminant []) ct pos
        else do
          return $ makeExprTyped
            (Identifier $ Var discriminant)
            (TypeEnumConstructor parentTp discriminant args (map snd params))
            pos
    VarBinding v ->
      return $ (makeExprTyped (Identifier $ Var $ varRealName v) (varType v) pos
               )
        { tIsLvalue         = True
        , tIsLocal          = varIsLocal v
        , tIsConst          = varIsConst v
        , tCompileTimeValue = case varDefault v of
          Just x  -> tCompileTimeValue x
          Nothing -> Nothing
        }
    FunctionBinding def -> do
      let t  = functionConcrete def
      let tp = functionRealName def
      if null $ functionParams def
        then return
          $ (makeExprTyped (Identifier $ Var tp) t pos) { tIsLvalue = True }
        else do
  -- TODO: allow specifying explicit function params
          params <- makeGeneric ctx tp pos []
          tctx   <- addTypeParams ctx tctx params (functionPos def)
          t      <- follow ctx tctx t
          let ft = case t of
                TypeFunction rt args varargs _ ->
                  TypeFunction rt args varargs (map snd params)
          return $ (makeExprTyped (Identifier $ Var tp) ft pos) { tIsLvalue = True
                                                                }
    -- TODO: these are invalid runtime values; don't abuse Identifier for them
    TypeBinding  t -> returnTypeBinding (typeName t) []
    TraitBinding t -> returnTraitBinding (traitName t) []
    ModuleBinding tp ->
      return $ makeExprTyped (Identifier (Var tp)) (ModuleType tp) pos
    TypedefBinding t modPath _ -> do
      mod  <- getMod ctx modPath
      tctx <- modTypeContext ctx mod
      t    <- resolveType ctx tctx mod t
      case t of
        TypeInstance tp params -> returnTypeBinding tp params
        TypeTraitConstraint (tp, params) -> returnTraitBinding tp params
        _                      -> invalidBinding
