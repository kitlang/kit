module Kit.Ast.Definitions (
  Definition(DefinitionVar, DefinitionFunction, DefinitionTrait, DefinitionType),
  module Kit.Ast.Definitions.Base,
  module Kit.Ast.Definitions.EnumVariant,
  module Kit.Ast.Definitions.FunctionDefinition,
  module Kit.Ast.Definitions.RewriteRule,
  module Kit.Ast.Definitions.TraitDefinition,
  module Kit.Ast.Definitions.TraitImplementation,
  module Kit.Ast.Definitions.TypeDefinition,
  module Kit.Ast.Definitions.VarDefinition
) where

import Kit.Ast.Definitions.Base
import Kit.Ast.Definitions.EnumVariant
import Kit.Ast.Definitions.FunctionDefinition
import Kit.Ast.Definitions.RewriteRule
import Kit.Ast.Definitions.TraitDefinition
import Kit.Ast.Definitions.TraitImplementation
import Kit.Ast.Definitions.TypeDefinition
import Kit.Ast.Definitions.VarDefinition

{-
  This follows the same `a b` parameter convention as ExprType, enabling easy
  conversions by providing expression and type converters.

  See convertVarDefinition, convertFunctionDefinition etc. to convert
  definitions at one stage of compilation to a later stage.
-}
data Definition a b
  = DefinitionVar (VarDefinition a b)
  | DefinitionFunction (FunctionDefinition a b)
  | DefinitionTrait (TraitDefinition a b)
  | DefinitionType (TypeDefinition a b)
  deriving (Eq, Show)
