module Kit.CodeGen.C.CDecl where

import Data.List
import Language.C
import Kit.Ast
import Kit.CodeGen.C.CExpr
import Kit.Ir
import Kit.Str

typeNameDecl name = CTypeSpec $ u $ CTypeDef $ internalIdent $ s_unpack name

cdecl :: BasicType -> [CDecl]
{- Kit struct = C struct -}
cdecl (BasicTypeStruct (name, fields)) =
  [ u $ CDecl
      [ CTypeSpec $ u $ CSUType $ u $ CStruct
          CStructTag
          (Just $ internalIdent $ s_unpack name)
          (Just f)
          []
      ]
      []
  ]
  where f = [ cDecl t (Just n) Nothing | (n, t) <- fields ]

{- Simple enums (no variant has any fields) will generate a C enum. -}
cdecl (BasicTypeSimpleEnum name variantNames) =
  [enumDiscriminant name variantNames]

{-
  Complex enums will generate the same C enum for the discriminant, but the
  enum's type will be a struct:

  {
    discriminant;
    union (one struct for each variant with fields);
  }

  Either type of enum can be cast to the discriminant's type.
-}
cdecl (BasicTypeComplexEnum name variants) =
  (enumDiscriminant discriminantName (map (\(name, _) -> name) variants))
    : (enum_struct name discriminantName variants)
    : (enumVariants name variants)
 where
  discriminantName = (s_concat [name, "_Discriminant"])
  enum_struct name discriminantName variants = u $ CDecl
    [ CTypeSpec $ u $ CSUType $ u $ CStruct
        CStructTag
        (Just $ internalIdent $ s_unpack name)
        (Just f)
        []
    ]
    []
   where
    f =
      [ u $ CDecl
        [ CTypeSpec $ u $ CEnumType $ u $ CEnum
            (Just (internalIdent $ s_unpack discriminantName))
            Nothing
            []
        ]
        [ ( Just $ u $ CDeclr (Just $ internalIdent "__discriminant")
                              []
                              Nothing
                              []
          , Nothing
          , Nothing
          )
        ]
      , u $ CDecl
        [ CTypeSpec $ u $ CSUType $ u $ CStruct CUnionTag
                                                Nothing
                                                (Just variant_fields)
                                                []
        ]
        [ ( Just $ u $ CDeclr (Just $ internalIdent "__variant") [] Nothing []
          , Nothing
          , Nothing
          )
        ]
      ]
    variant_fields =
      [ u $ CDecl
          [typeNameDecl $ enumVariantName name variantName]
          [ ( Just $ u $ CDeclr
              (Just $ internalIdent $ s_unpack $ s_concat
                ["variant_", variantName]
              )
              []
              Nothing
              []
            , Nothing
            , Nothing
            )
          ]
      | (variantName, _) <- nonemptyVariants
      ]
  enumVariants name variants = map
    (\(variantName, variant_fields) ->
      (cdecl
          (BasicTypeStruct (enumVariantName name variantName, variant_fields))
        )
        !! 0
    )
    nonemptyVariants
  nonemptyVariants = filter (\(name, fields) -> fields /= []) variants

enumDiscriminant name variantNames = u $ CDecl
  [ CTypeSpec $ u $ CEnumType $ u $ CEnum
      (Just $ internalIdent $ s_unpack name)
      (Just [ (internalIdent $ s_unpack v, Nothing) | v <- variantNames ])
      []
  ]
  []

enumVariantName enum_name variantName =
  s_concat [enum_name, "_Variant_", variantName]
