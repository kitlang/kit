module Kit.Compiler.Generators.C.CFun where

import Data.Maybe
import Language.C
import Kit.Ast
import Kit.Compiler.Generators.C.CExpr
import Kit.NameMangling
import Kit.Ir
import Kit.Str

cfunDecl :: FunctionDefinition IrExpr BasicType -> CDecl
cfunDecl f@(FunctionDefinition { functionName = name, functionType = rt, functionArgs = args, functionVararg = vararg })
  = u $ CDecl
    ((specFromMeta $ functionMeta f) ++ typeSpec)
    [ ( Just $ u $ CDeclr
        (Just $ cIdent $ mangleName name)
        ( (u $ CFunDeclr (Right (map cfunArg args, isJust vararg)) [])
        : derivedDeclr
        )
        Nothing
        (attributesFromMeta $ functionMeta f)
      , Nothing
      , Nothing
      )
    ]
  where (typeSpec, derivedDeclr) = ctype rt

cfunDef :: FunctionDefinition IrExpr BasicType -> CFunDef
cfunDef f@(FunctionDefinition { functionName = name, functionType = rt, functionArgs = args, functionVararg = vararg, functionBody = Just body })
  = u $ CFunDef
    ((specFromMeta $ functionMeta f) ++ typeSpec)
    (u $ CDeclr
      (Just $ cIdent $ mangleName name)
      ( (u $ CFunDeclr (Right (map cfunArg args, isJust vararg)) [])
      : derivedDeclr
      )
      Nothing
      []
    )
    []
    (transpileStmt body)
  where (typeSpec, derivedDeclr) = ctype rt

cfunArg :: ArgSpec IrExpr BasicType -> CDecl
cfunArg arg = cDecl (argType arg) (Just ([], argName arg)) Nothing

attributesFromMeta :: [Metadata] -> [CAttr]
attributesFromMeta (h : t) = case metaName h of
  "inline" ->
    (u $ CAttr (internalIdent "always_inline") []) : attributesFromMeta t
  "noreturn" ->
    (u $ CAttr (internalIdent "noreturn") []) : attributesFromMeta t
  _ -> attributesFromMeta t
attributesFromMeta [] = []

specFromMeta :: [Metadata] -> [CDeclSpec]
specFromMeta (h : t) = case metaName h of
  "static" -> (CStorageSpec $ u $ CStatic) : specFromMeta t
  _        -> specFromMeta t
specFromMeta [] = []
