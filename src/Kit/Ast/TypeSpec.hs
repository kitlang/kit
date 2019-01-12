module Kit.Ast.TypeSpec (
  module Kit.Ast.TypeSpecBase,
  TypeSpec,
) where

import Kit.Ast.TypeSpecBase
import Kit.Ast.ConcreteType

type TypeSpec = TypeSpecBase ConcreteType
