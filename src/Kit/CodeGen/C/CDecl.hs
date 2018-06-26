module Kit.CodeGen.C.CDecl where

  import Data.List
  import Language.C
  import Kit.Ast
  import Kit.CodeGen.C.CExpr
  import Kit.Str

  param_name :: ParameterizedType -> String
  -- TODO
  param_name p = show p

  monomorph_suffix :: [ParameterizedType] -> String
  monomorph_suffix tp = intercalate "_" (map param_name tp)

  cstruct_name :: ModulePath -> [ParameterizedType] -> Structure -> String
  cstruct_name m tp s = (intercalate "_" $ map s_unpack m) ++ "__" ++ (s_unpack $ structure_name s) ++ (if length tp > 0 then "__" else "") ++ (monomorph_suffix tp)

  cdecl :: BasicType -> [CDecl]
  {- Kit struct = C struct -}
  cdecl (TypeStruct (name, fields))
    = [u $ CDecl [
          CStorageSpec $ u $ CTypedef,
          CTypeSpec $ u $ CSUType $ u $ CStruct CStructTag Nothing (Just f) []
        ] [(Just $ u $ CDeclr (Just $ internalIdent $ s_unpack name) [] Nothing [], Nothing, Nothing)]]
      where f = [u $ CDecl (ctype (BasicType t)) [(Just $ u $ CDeclr (Just $ internalIdent $ s_unpack n) [] Nothing [], Nothing, Nothing)] | (n, t) <- fields]

  {- Simple enums (no variant has any fields) will generate a C enum. -}
  cdecl (TypeSimpleEnum name variant_names)
    = [enum_discriminant name variant_names]

  {-
    Complex enums will generate the same C enum for the discriminant, but the
    enum's type will be a struct:

    {
      discriminant;
      union (one struct for each variant with fields);
    }

    Either type of enum can be cast to the discriminant's type.
  -}
  cdecl (TypeComplexEnum name variants)
    = (enum_discriminant discriminant_name (map (\(name, _) -> name) variants)) : (enum_struct name discriminant_name variants) : (enum_variants name variants)
    where
      discriminant_name = (s_concat [name, "_Discriminant"])
      enum_struct name discriminant_name variants
        = u $ CDecl [
              CStorageSpec $ u $ CTypedef,
              CTypeSpec $ u $ CSUType $ u $ CStruct CStructTag Nothing (Just f) []
            ] [(Just $ u $ CDeclr (Just $ internalIdent $ s_unpack name) [] Nothing [], Nothing, Nothing)]
          where f = [
                      u $ CDecl [type_name_decl discriminant_name] [(Just $ u $ CDeclr (Just $ internalIdent "__discriminant") [] Nothing [], Nothing, Nothing)],
                      u $ CDecl [CTypeSpec $ u $ CSUType $ u $ CStruct CUnionTag Nothing (Just variant_fields) []] [(Just $ u $ CDeclr (Just $ internalIdent "__variant") [] Nothing [], Nothing, Nothing)]
                    ]
                variant_fields = [u $ CDecl [type_name_decl $ enum_variant_name name variant_name] [(Just $ u $ CDeclr (Just $ internalIdent $ s_unpack $ s_concat ["variant_", variant_name]) [] Nothing [], Nothing, Nothing)] | (variant_name, _) <- nonempty_variants]
      enum_variants name variants =
        map (\(variant_name, variant_fields) -> (cdecl (TypeStruct (enum_variant_name name variant_name, variant_fields))) !! 0) nonempty_variants
      nonempty_variants = filter (\(name, fields) -> fields /= []) variants

  enum_discriminant name variant_names
    = u $ CDecl [
                  CTypeSpec $ u $ CEnumType $ u $ CEnum (Just $ internalIdent $ s_unpack name) (Just [(internalIdent $ s_unpack v, Nothing) | v <- variant_names]) []
                ] []

  enum_variant_name enum_name variant_name = s_concat [enum_name, "_Variant_", variant_name]
