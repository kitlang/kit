module Kit.CodeGen.C.CFun where

import Language.C
import Kit.Ast
import Kit.CodeGen.C.CExpr
import Kit.Ir
import Kit.Str

cfunDecl :: Str -> BasicType -> CDecl
cfunDecl name (BasicTypeFunction rt args varargs) = u $ CDecl
  (map CTypeSpec $ typeSpec)
  [ ( Just $ u $ CDeclr
      (Just $ internalIdent $ s_unpack name)
      ((u $ CFunDeclr (Right (map cfunArg args, varargs)) []) : derivedDeclr)
      Nothing
      []
    , Nothing
    , Nothing
    )
  ]
  where (typeSpec, derivedDeclr) = ctype rt

cfunDef :: Str -> BasicType -> IrExpr -> CFunDef
cfunDef name (BasicTypeFunction rt args varargs) body = u $ CFunDef
  (map CTypeSpec $ typeSpec)
  (u $ CDeclr
    (Just $ internalIdent $ s_unpack name)
    ((u $ CFunDeclr (Right (map cfunArg args, varargs)) []) : derivedDeclr)
    Nothing
    []
  )
  []
  (transpileStmt body)
  where (typeSpec, derivedDeclr) = ctype rt

cfunArg :: (Str, BasicType) -> CDecl
cfunArg (argName, argType) = cDecl argType (Just argName) Nothing
