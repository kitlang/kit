module Kit.Ast.TypeConstraint where

import Data.List
import Kit.Ast.BasicType
import Kit.Ast.ConcreteType
import Kit.Ast.ModulePath
import Kit.Parser.Span

type UnresolvedTypeConstraint = ConcreteType -> TypeConstraint

data TypeConstraint
  = TypeEq ConcreteType ConcreteType String Span
  deriving (Eq, Show)

data TypeInformation
  = TypeVarIs TypeVar ConcreteType
  | TypeVarConstraint TypeVar TraitConstraint
  | TypeConstraintSatisfied
  | TypeConstraintNotSatisfied

instance Eq TypeInformation where
  (==) (TypeVarIs a b) (TypeVarIs c d) = (a == c) && (b == d)
  (==) (TypeVarConstraint a b) (TypeVarConstraint c d) = (a == c) && (b == d)
  (==) TypeConstraintSatisfied TypeConstraintSatisfied = True
  (==) TypeConstraintNotSatisfied TypeConstraintNotSatisfied = True
  (==) a b = False

instance Show TypeInformation where
  show (TypeVarIs a b) = (show a) ++ " := " ++ (show b)
  show TypeConstraintSatisfied = "satisfied"
  show TypeConstraintNotSatisfied = "not satisfied"
  show (TypeVarConstraint tv constraint) = (show tv) ++ " => " ++ show constraint

data TypeVarInfo = TypeVarInfo
  { typeVarValue :: Maybe ConcreteType
  , typeVarConstraints :: [(TraitConstraint, (String, Span))]
  , typeVarPositions :: [Span]
  } deriving (Eq, Show)

newTypeVarInfo p = TypeVarInfo
  { typeVarValue       = Nothing
  , typeVarConstraints = []
  , typeVarPositions   = [p]
  }
addTypeVarPosition info p =
  info { typeVarPositions = p : typeVarPositions info }
addTypeVarConstraints info c reason pos = info
  { typeVarConstraints = nubBy (\(c1, _) (c2, _) -> c1 == c2)
                               ((c, (reason, pos)) : typeVarConstraints info)
  }

typeClassNumeric = TypeTraitConstraint ((["kit", "numeric"], "Numeric"), [])
typeClassIntegral = TypeTraitConstraint ((["kit", "numeric"], "Integral"), [])
typeClassNumericMixed =
  TypeTraitConstraint ((["kit", "numeric"], "NumericMixed"), [])
typeClassIterable v =
  TypeTraitConstraint ((["kit", "iterator"], "Iterable"), [])
typeClassStringy = TypeTraitConstraint ((["kit", "string"], "Stringy"), [])
