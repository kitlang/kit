module Kit.Ast.TypeConstraint where

import Data.List
import Kit.Ast.TypePath
import Kit.Ast.Types
import Kit.Ast.Span
import Kit.Str

type UnresolvedTypeConstraint = ConcreteType -> TypeConstraint

data TypeConstraint
  = TypeEq ConcreteType ConcreteType String Span
  -- | TypeNotVoid
  deriving (Eq, Show)

data TypeInformation
  = TypeVarIs TypeVar ConcreteType
  | TypeVarConstraint TypeVar TraitConstraint

instance Eq TypeInformation where
  (==) (TypeVarIs a b) (TypeVarIs c d) = (a == c) && (b == d)
  (==) (TypeVarConstraint a b) (TypeVarConstraint c d) = (a == c) && (b == d)
  (==) a b = False

instance Show TypeInformation where
  show (TypeVarIs a b) = (show a) ++ " := " ++ (show b)
  show (TypeVarConstraint tv constraint) = (show tv) ++ " => " ++ show constraint

data TypeVarInfo = TypeVarInfo
  { typeVarId :: Int
  , typeVarValue :: Maybe ConcreteType
  , typeVarConstraints :: [(TraitConstraint, (String, Span))]
  , typeVarPositions :: [Span]
  } deriving (Eq)

instance Show TypeVarInfo where
  show (TypeVarInfo {typeVarValue = Just v, typeVarConstraints = l}) = show v
  show (TypeVarInfo {typeVarValue = Nothing, typeVarConstraints = l}) = "Unbound type variable implementing " ++ _showConstraints l

_showConstraints [] = ""
_showConstraints l =
  "(" ++ intercalate ", " (map (s_unpack . _showConstraint . fst) l) ++ ")"
_showConstraint (tp, _) = showTypePath tp

newTypeVarInfo id p = TypeVarInfo
  { typeVarId          = id
  , typeVarValue       = Nothing
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
typeClassRange exp = TypeTraitConstraint
  ( ( ["kit", "numeric"]
    , s_concat
      [if exp < 0 then "NumericNE" else "NumericE", s_pack $ show $ abs exp]
    )
  , []
  )
typeClassNumericMixed = TypeTraitConstraint (typeClassNumericMixedPath, [])
typeClassIterable v = TypeTraitConstraint (typeClassIterablePath, [v])

typeClassNumericPath :: TypePath
typeClassNumericPath = (["kit", "numeric"], "Numeric")
typeClassIntegralPath :: TypePath
typeClassIntegralPath = (["kit", "numeric"], "Integral")
typeClassNumericMixedPath :: TypePath
typeClassNumericMixedPath = (["kit", "numeric"], "NumericMixed")
typeClassIterablePath :: TypePath
typeClassIterablePath = (["kit", "iterator"], "Iterable")
typeClassIteratorPath :: TypePath
typeClassIteratorPath = (["kit", "iterator"], "Iterator")
typeOptionPath :: TypePath
typeOptionPath = (["kit", "option"], "Option")
