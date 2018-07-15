module Kit.CodeGen.C.CFun where

import Language.C
import Kit.Ast
import Kit.CodeGen.C.CExpr
import Kit.Ir
import Kit.Str

cfunDecl :: Str -> BasicType -> CDecl
cfunDecl name (BasicTypeFunction rt args varargs) = u $ CDecl
  (map CTypeSpec $ ctype rt)
  [ ( Just $ u $ CDeclr (Just $ internalIdent $ s_unpack name)
                        [u $ CFunDeclr (Right (map cfunArg args, varargs)) []]
                        Nothing
                        []
    , Nothing
    , Nothing
    )
  ]

cfunDef :: Str -> BasicType -> IrExpr -> CFunDef
cfunDef name (BasicTypeFunction rt args varargs) body = u $ CFunDef
  (map CTypeSpec $ ctype rt)
  (u $ CDeclr (Just $ internalIdent $ s_unpack name)
              [u $ CFunDeclr (Right (map cfunArg args, varargs)) []]
              Nothing
              []
  )
  []
  (transpileStmt body)

cfunArg :: (Str, BasicType) -> CDecl
cfunArg (argName, argType) = u $ CDecl
  (map CTypeSpec $ ctype argType)
  [ ( Just $ u $ CDeclr (Just $ internalIdent $ s_unpack argName) [] Nothing []
    , Nothing
    , Nothing
    )
  ]
