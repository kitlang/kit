module Kit.CodeGen.C where

  import Numeric
  import Language.C
  import Kit.Str
  import Kit.Ast.Expr
  import Kit.Ast.Operator
  import Kit.Ast.Type
  import Kit.Ast.Value

  u x = x undefNode
  ct x = CTypeSpec $ u x

  ctype :: ConcreteType -> [CDeclSpec]
  ctype (BasicType TypeVoid) = [ct CVoidType]
  ctype (BasicType (TypeInt 8)) = [ct CSignedType, ct CCharType]
  ctype (BasicType (TypeInt 16)) = [ct CSignedType, ct CShortType]
  ctype (BasicType (TypeInt 32)) = [ct CSignedType, ct CLongType]
  ctype (BasicType (TypeInt 64)) = [ct CSignedType, ct CLongType, ct CLongType]
  ctype (BasicType (TypeUint 8)) = [ct CUnsigType, ct CCharType]
  ctype (BasicType (TypeUint 16)) = [ct CUnsigType, ct CShortType]
  ctype (BasicType (TypeUint 32)) = [ct CUnsigType, ct CLongType]
  ctype (BasicType (TypeUint 64)) = [ct CUnsigType, ct CLongType, ct CLongType]
  ctype (BasicType (TypeFloat 32)) = [ct CFloatType]
  ctype (BasicType (TypeFloat 64)) = [ct CDoubleType]

  --transpile :: [Expr] -> [CStat]
  --transpile exprs

  transpile_expr :: Expr -> CExpr
  transpile_expr (Expr {expr = Lvalue (Var s)}) = u $ CVar $ internalIdent $ s_unpack s
  transpile_expr (Expr {expr = Literal (BoolValue b)}) = CConst $ u $ CIntConst $ cInteger (if b then 1 else 0)
  transpile_expr (Expr {expr = Literal (IntValue i)}) = CConst $ u $ CIntConst $ transpile_int (s_unpack i)
  transpile_expr (Expr {expr = Literal (FloatValue f)}) = CConst $ u $ CFloatConst $ transpile_float (s_unpack f)
  transpile_expr (Expr {expr = Binop op e1 e2}) = u $ CBinary (transpile_binop op) (transpile_expr e1) (transpile_expr e2)
  transpile_expr (Expr {expr = PreUnop op e1}) = u $ CUnary (transpile_pre_unop op) (transpile_expr e1)
  transpile_expr (Expr {expr = PostUnop op e1}) = u $ CUnary (transpile_post_unop op) (transpile_expr e1)
  -- TODO: -> ?
  transpile_expr (Expr {expr = Field e s}) = u $ CMember (transpile_expr e) (internalIdent $ s_unpack s) False
  transpile_expr (Expr {expr = ArrayAccess e1 e2}) = u $ CIndex (transpile_expr e1) (transpile_expr e2)
  transpile_expr (Expr {expr = Call e args}) = u $ CCall (transpile_expr e) [transpile_expr x | x <- args]
  transpile_expr (Expr {expr = Cast e _, expr_type = Just t}) = u $ CCast (u $ CDecl (ctype t) []) (transpile_expr e)
  --transpile_expr (Expr {expr = VectorLiteral e}) = u $ CA

  transpile_stmt :: Expr -> CStat
  transpile_stmt (Expr {expr = Break}) = u CBreak
  transpile_stmt (Expr {expr = Continue}) = u CCont
  transpile_stmt (Expr {expr = Return (Just r)}) = u $ CReturn $ Just $ transpile_expr r
  transpile_stmt (Expr {expr = Return Nothing}) = u $ CReturn Nothing
  transpile_stmt (Expr {expr = Block e}) = u $ CCompound [] [transpile_block_item x | x <- e]
  transpile_stmt (Expr {expr = If cond e1 (Just e2)}) = u $ CIf (transpile_expr cond) (transpile_stmt e1) (Just $ transpile_stmt e2)
  transpile_stmt (Expr {expr = If cond e1 Nothing}) = u $ CIf (transpile_expr cond) (transpile_stmt e1) (Nothing)
  transpile_stmt (Expr {expr = While cond e}) = u $ CWhile (transpile_expr cond) (transpile_stmt e) False
  transpile_stmt (Expr {expr = For (Expr {expr = Lvalue (Var v), expr_type = Just id_type}) (Expr {expr = RangeLiteral start end}) e2}) = u $ CFor (Right $ u $ CDecl (ctype id_type) [(Just $ var_to_cdeclr v, Just $ u $ CInitExpr $ transpile_expr start, Nothing)]) (Just $ transpile_expr $ e $ Binop Lt (e $ Lvalue (Var v)) (end)) (Just $ transpile_expr $ e $ PreUnop Inc (e $ Lvalue (Var v))) (transpile_stmt e2)
  transpile_stmt e = u $ CExpr $ Just $ transpile_expr e

  var_to_cdeclr x = u $ CDeclr (Just $ internalIdent $ s_unpack x) [] (Nothing) []

  transpile_block_item :: Expr -> CBlockItem
  transpile_block_item (Expr {expr = (VarDef v), expr_type = Just t}) =
    CBlockDecl $ u $ CDecl (ctype t) [(Just vn, body, Nothing)]
    where vn = case var_name v of
                 Var x -> var_to_cdeclr x
          body = case var_default v of
                   Just x -> Just $ u $ CInitExpr $ transpile_expr x
                   Nothing -> Nothing
  transpile_block_item x = CBlockStmt $ transpile_stmt x

  -- TODO: validate
  r1 x = fst (x !! 0)

  transpile_int ('0':'x':s) = cInteger $ r1 $ readHex s
  transpile_int ('0':'b':s) = cInteger $ r1 $ readInt 2 isBin readBin s
                              where isBin x = (x == '0' || x == '1')
                                    readBin '0' = 0
                                    readBin '1' = 1
  transpile_int ('0':'o':s) = cInteger $ r1 $ readOct ('0':s)
  transpile_int s = cInteger $ r1 $ readDec s

  transpile_float s = cFloat $ r1 $ readFloat s

  transpile_binop Add = CAddOp
  transpile_binop Sub = CSubOp
  transpile_binop Mul = CMulOp
  transpile_binop Div = CDivOp
  transpile_binop Mod = CRmdOp
  transpile_binop Eq = CEqOp
  transpile_binop Neq = CNeqOp
  transpile_binop Gt = CGrOp
  transpile_binop Lt = CLeOp
  transpile_binop Gte = CGeqOp
  transpile_binop Lte = CLeqOp
  transpile_binop LeftShift = CShlOp
  transpile_binop RightShift = CShrOp
  transpile_binop And = CLndOp
  transpile_binop Or = CLorOp
  transpile_binop BitAnd = CAndOp
  transpile_binop BitOr = COrOp
  transpile_binop BitXor = CXorOp

  transpile_pre_unop Inc = CPreIncOp
  transpile_pre_unop Dec = CPreDecOp
  transpile_pre_unop Sub = CMinOp
  transpile_pre_unop Invert = CNegOp
  transpile_pre_unop InvertBits = CCompOp

  transpile_post_unop Inc = CPostIncOp
  transpile_post_unop Dec = CPostDecOp
