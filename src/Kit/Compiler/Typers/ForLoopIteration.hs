module Kit.Compiler.Typers.ForLoopIteration where

import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.DumpAst
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Parser
import Kit.Str

iterationTransform :: TypedExpr -> ConcreteType -> Span -> TypedExpr -> TypedExpr -> TypedExpr
iterationTransform box tv pos idExpr inner =
  let optionType = TypeInstance typeOptionPath [tv]
  in
    let iteratorType = TypeBox typeClassIteratorPath [tv]
    in
      let iterableType = TypeBox typeClassIterablePath [tv]
      in
        makeExprTyped
          (Block
            [ makeExprTyped
              (VarDeclaration
                (Var ([], "__iter"))
                (iteratorType)
                (Just $ makeExprTyped
                  (Call
                    (makeExprTyped (Field box (Var ([], "iterator")))
                                   (voidType)
                                   pos
                    )
                    []
                  )
                  iteratorType
                  pos
                )
              )
              iteratorType
              pos
            , makeExprTyped
              (VarDeclaration (Var ([], "__current")) (optionType) Nothing)
              optionType
              pos
            , makeExprTyped
              (While
                (makeExprTyped (Literal $ BoolValue True)
                               (TypeBasicType $ BasicTypeBool)
                               pos
                )
                (makeExprTyped
                  (Block
                    [ makeExprTyped
                      (Binop
                        Assign
                        (makeExprTyped
                          (TupleInit
                            [ makeExprTyped (Identifier $ Var ([], "__iter"))
                                            iteratorType
                                            pos
                            , makeExprTyped
                              (Identifier $ Var ([], "__current"))
                              optionType
                              pos
                            ]
                          )
                          (TypeTuple [iteratorType, optionType])
                          pos
                        )
                        (makeExprTyped
                          (Call
                            (makeExprTyped
                              (Field
                                (makeExprTyped
                                  (Identifier $ Var ([], "__iter"))
                                  iteratorType
                                  pos
                                )
                                (Var ([], "next"))
                              )
                              (TypeTuple [iteratorType, optionType])
                              pos
                            )
                            []
                          )
                          iteratorType
                          pos
                        )
                      )
                      voidType
                      pos
                    , makeExprTyped
                      (Match
                        (makeExprTyped (Identifier $ Var ([], "__current"))
                                       voidType
                                       pos
                        )
                        [ matchCase
                            (makeExprTyped
                              (Call
                                (makeExprTyped
                                  (Identifier $ Var $ subPath (typeOptionPath)
                                                              "Some"
                                  )
                                  voidType
                                  pos
                                )
                                [idExpr]
                              )
                              voidType
                              pos
                            )
                            inner
                        ]
                        (Just $ makeExprTyped Break voidType pos)
                      )
                      voidType
                      pos
                    ]
                  )
                  voidType
                  pos
                )
                True
              )
              voidType
              pos
            ]
          )
          voidType
          pos
