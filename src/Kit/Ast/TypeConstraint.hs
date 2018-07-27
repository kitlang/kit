module Kit.Ast.TypeConstraint where

import Data.List
import Kit.Ast.BasicType
import Kit.Ast.ConcreteType
import Kit.Ast.ModulePath
import Kit.Parser.Span
import Kit.Str

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
  } deriving (Eq)

instance Show TypeVarInfo where
  show (TypeVarInfo {typeVarValue = Just v, typeVarConstraints = l}) = show v
  show (TypeVarInfo {typeVarValue = Nothing, typeVarConstraints = l}) = "Unbound type variable implementing " ++ _showConstraints l

_showConstraints [] = ""
_showConstraints l = "(" ++ intercalate ", " (map (s_unpack . _showConstraint . fst) l) ++ ")"
_showConstraint (tp, _) = showTypePath tp

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
