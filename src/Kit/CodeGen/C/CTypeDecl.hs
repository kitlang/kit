module Kit.CodeGen.C.CTypeDecl where

import Data.List
import Language.C
import Kit.Ast
import Kit.CodeGen.C.CExpr
import Kit.Ir
import Kit.Str

typeNameDecl name = CTypeSpec $ u $ CTypeDef $ internalIdent $ s_unpack name

makeSUFields fields = [ cDecl t (Just n) Nothing | (n, t) <- fields ]

cTypeDecl :: TypeDefinition IrExpr BasicType -> [CDecl]
cTypeDecl t@(TypeDefinition { typeName = name }) = case typeSubtype t of
  Struct { structFields = fields } ->
    [ u $ CDecl
        [ CTypeSpec $ u $ CSUType $ u $ CStruct
            CStructTag
            (Just $ internalIdent $ s_unpack name)
            (Just $ makeSUFields [(varName field, varType field) | field <- fields])
            []
        ]
        []
    ]

  Union { unionFields = fields } ->
    [ u $ CDecl
        [ CTypeSpec $ u $ CSUType $ u $ CStruct
            CUnionTag
            (Just $ internalIdent $ s_unpack name)
            (Just $ makeSUFields [(varName field, varType field) | field <- fields])
            []
        ]
        []
    ]

  {- Simple enums (no variant has any fields) will generate a C enum. -}
  e@(Enum { enumVariants = variants }) | enumIsSimple e ->
    [enumDiscriminant (Just name) (map variantName variants)]

  {-
    Complex enums will generate the same C enum for the discriminant, but the
    enum's type will be a struct:

    {
      discriminant;
      union (one struct for each variant with fields);
    }

    Either type of enum can be cast to the discriminant's type.
  -}
  e@(Enum { enumVariants = variants }) ->
    (declareEnumVariants name variants)
      ++ [ (enumDiscriminant (Just discriminantName)
                             (map variantName variants)
           )
         , (enum_struct name discriminantName variants)
         ]
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
    f =
      [ u $ CDecl
        [ CTypeSpec $ u $ CEnumType $ u $ CEnum
            (Just (internalIdent $ s_unpack discriminantName))
            Nothing
            []
        ]
        [ ( Just $ u $ CDeclr
            (Just $ internalIdent $ s_unpack discriminantFieldName)
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
        [ ( Just $ u $ CDeclr
            (Just $ internalIdent $ s_unpack variantFieldName)
            []
            Nothing
            []
          , Nothing
          , Nothing
          )
        ]
      ]
    variant_fields =
      [ u $ CDecl
          [ CTypeSpec $ u $ CSUType $ u $ CStruct
              CStructTag
              (Just $ internalIdent $ s_unpack $ enumVariantName
                name
                (variantName variant)
              )
              Nothing
              []
          ]
          [ ( Just $ u $ CDeclr
              (Just $ internalIdent $ s_unpack $ s_concat
                ["variant_", (variantName variant)]
              )
              []
              Nothing
              []
            , Nothing
            , Nothing
            )
          ]
      | variant <- nonemptyVariants
      ]
    declareEnumVariants name variants = map
      (\variant -> head
        (cTypeDecl
          (newTypeDefinition
            { typeName    = enumVariantName name (variantName variant)
            , typeSubtype = Struct
              { structFields = [ newVarDefinition { varName = argName arg
                                                  , varType = argType arg
                                                  }
                               | arg <- (variantArgs variant)
                               ]
              }
            }
          )
        )
      )
      nonemptyVariants
    nonemptyVariants =
      filter (\variant -> not $ null $ variantArgs variant) variants

  _ -> []

enumDiscriminant name variantNames = u $ CDecl
  [ CTypeSpec $ u $ CEnumType $ u $ CEnum
      (case name of
        Just name -> Just $ internalIdent $ s_unpack name
        Nothing   -> Nothing
      )
      (Just [ (internalIdent $ s_unpack v, Nothing) | v <- variantNames ])
      []
  ]
  []

enumVariantName enum_name variantName =
  s_concat [enum_name, "_Variant_", variantName]

cTupleDecl name slots =
  [ u $ CDecl
      [ CTypeSpec $ u $ CSUType $ u $ CStruct
          CStructTag
          (Just $ internalIdent $ s_unpack name)
          (Just $ makeSUFields
            [ (s_pack $ "__slot" ++ show i, slot)
            | (i, slot) <- zip [0 ..] slots
            ]
          )
          []
      ]
      []
  ]
