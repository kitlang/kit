module Kit.Compiler.Ir.DeclarationToIr where

import Control.Monad
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Ir.FindUnderlyingType
import Kit.Compiler.Ir.ExprToIr
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedStmt
import Kit.Compiler.Utils
import Kit.Ir
import Kit.NameMangling
import Kit.Str

generateDeclIr :: CompileContext -> Module -> TypedStmt -> IO [IrBundle]
generateDeclIr ctx mod t = do
  ictx <- newIrContext
  let converter' = converter (typedToIr ctx ictx mod)
                             (\pos -> findUnderlyingType ctx mod (Just pos))
  let paramConverter = \p -> converter'
  case stmt t of
    TypeDeclaration def' -> do
      let
        def =
          def' { typeName = monomorphName (typeName def') (typeMonomorph def') }
      let name = typeName def

      debugLog ctx $ "generating IR for type " ++ (s_unpack $ showTypePath name)
      converted <- convertTypeDefinition paramConverter $ def { typeRules = [] }
      staticFields <- forM (typeStaticFields def)
                           (\field -> generateDeclIr ctx mod $ varDecl field)
      staticMethods <- forM
        (typeStaticMethods def)
        (\method -> generateDeclIr ctx mod $ functionDecl method)
      instanceMethods <- forM
        (typeMethods def)
        (\method -> generateDeclIr ctx mod $ functionDecl method)
      subtype <- case typeSubtype converted of
        t@(Enum { enumVariants = variants }) -> do
          let newName n =
                if modIsCModule mod then n else subPath name (tpName n)
          let variants' =
                [ variant { variantName = newName $ variantName variant }
                | variant <- variants
                ]
          return $ t { enumVariants = variants' }
        x -> return x

      return
        $ [ foldr
              (\b acc -> mergeBundles acc b)
              (IrBundle (typeName def')
                        [typeDecl $ converted { typeSubtype = subtype }]
              )
              (foldr (++) [] $ staticFields ++ staticMethods ++ instanceMethods)
          ]

    FunctionDeclaration f' -> do
      let f = f'
            { functionName = monomorphName (functionName f')
                                           (functionMonomorph f')
            }
      let name = functionName f
      debugLog ctx
        $  "generating IR for function "
        ++ (s_unpack $ showTypePath name)

      let isMain =
            (functionName f == ([], "main"))
              && (ctxMainModule ctx == modPath mod)
              && not (ctxIsLibrary ctx)

      converted <- convertFunctionDefinition paramConverter f
      let body = case (functionBody converted, functionVararg f) of
            (Just body, Just vararg) -> Just $ IrBlock
              [ IrVarDeclaration vararg (BasicTypeTypedef "va_list") Nothing
              , IrCall
                (IrIdentifier ([], "va_start"))
                [ IrIdentifier ([], vararg)
                , IrIdentifier $ ([], argName $ last $ functionArgs f)
                ]
              , body
              ]
            _ -> functionBody converted

      if (isMain && functionType converted == BasicTypeVoid)
      then
        return
          $ [ IrBundle
                name
                ([ functionDecl $ converted
                     { functionName = name
                     , functionType = BasicTypeCInt
                     , functionBody = case body of
                       Just x ->
                         Just
                           $ IrBlock
                               [ x
                               , IrReturn $ Just $ IrLiteral (IntValue 0)
                                                             BasicTypeCInt
                               ]
                       Nothing ->
                         Just
                           (IrReturn $ Just $ IrLiteral (IntValue 0)
                                                        BasicTypeCInt
                           )
                     }
                 ]
                )
            ]
      else
        return
          $ [ IrBundle
                (case functionBundle f of
                  Just x -> x
                  _      -> tpShift name
                )
                [ functionDecl $ converted
                    { functionType = if isMain
                      then BasicTypeCInt
                      else functionType converted
                    , functionBody = body
                    }
                ]
            ]

    VarDeclaration v@(VarDefinition { varName = name }) -> do
      debugLog ctx $ "generating IR for var " ++ (s_unpack $ showTypePath name)
      converted <- convertVarDefinition converter' v
      return
        $ [ IrBundle
              (case varBundle v of
                Just x -> x
                _      -> tpShift $ varName converted
              )
              [varDecl converted]
          ]

    TraitDeclaration (TraitDefinition { traitMethods = [], traitStaticFields = [], traitStaticMethods = [] })
      -> return []
    TraitDeclaration trait' -> do
      let trait = trait'
            { traitName  = monomorphName (traitName trait')
                                         (traitMonomorph trait')
            , traitRules = []
            }
      let name = traitName trait
      debugLog ctx
        $  "generating IR for trait "
        ++ (s_unpack $ showTypePath name)
      tctx      <- modTypeContext ctx mod
      converted <- convertTraitDefinition paramConverter trait
      -- trait declarations become struct definitions for the box/vtable
      let boxName    = subPath name "box"
      let vtableName = subPath name "vtable"
      let
        traitBox = newTypeDefinition
          { typeName    = boxName
          , typeSubtype = Struct
            { structFields = [ newVarDefinition
                               { varName = ([], valuePointerName)
                               , varType = CPtr BasicTypeVoid
                               }
                             , newVarDefinition
                               { varName = ([], vtablePointerName)
                               , varType = CPtr $ BasicTypeStruct vtableName
                               }
                             ]
            }
          }
      let
        vtable = newTypeDefinition
          { typeName    = vtableName
          , typeSubtype = Struct
            { structFields = [ newVarDefinition
                                 { varName = ([], tpName $ functionName f)
                                 , varType = BasicTypeFunction
                                   (functionType f)
                                   ( (vThisArgName, CPtr BasicTypeVoid)
                                   : [ (argName arg, argType arg)
                                     | arg <- functionArgs f
                                     ]
                                   )
                                   (isJust $ functionVararg f)
                                 }
                             | f <- traitMethods converted
                             ]
              ++ [ f { varName = ([], tpName $ varName f) }
                 | f <- traitStaticFields converted
                 ]
              ++ [ newVarDefinition
                     { varName = ([], tpName $ functionName f)
                     , varType = BasicTypeFunction
                       (functionType f)
                       [ (argName arg, argType arg) | arg <- functionArgs f ]
                       (isJust $ functionVararg f)
                     }
                 | f <- traitStaticMethods converted
                 ]
            }
          }

      return
        $ [IrBundle (traitName trait') [typeDecl traitBox, typeDecl vtable]]

    Implement (TraitImplementation { implMethods = [], implStaticFields = [], implStaticMethods = [] })
      -> return []
    Implement i'@(TraitImplementation { implTrait = TypeTraitConstraint (traitName, traitParams), implFor = ct })
      -> do
        tctx        <- modTypeContext ctx mod
        traitParams <- forM traitParams $ mapType (follow ctx tctx)
        let
          i = i'
            { implName = monomorphName (monomorphName traitName traitParams)
                                       [ct]
            }
        for <- findUnderlyingType ctx mod (Just $ implPos i) ct
        let name       = monomorphName (traitName) traitParams
        let vtableName = subPath name "vtable"
        methods <- forM (implMethods i) $ \method -> do
          f' <- convertFunctionDefinition paramConverter method
          let
            f = implicitifyMethod
              vThisArgName
              (CPtr BasicTypeVoid)
              (\_ x -> IrBlock
                [ IrVarDeclaration
                  thisPtrName
                  (CPtr for)
                  (Just $ IrCast (IrIdentifier ([], vThisArgName)) (CPtr for))
                , x
                ]
              )
              f'
          let name' = subPath (implName i) $ tpName $ functionName f
          return (name', functionDecl $ f { functionName = name' })
        let
          impl = newVarDefinition
            { varName    = implName i
            , varType    = BasicTypeStruct vtableName
            , varMeta    = [meta metaConst]
            , varDefault = Just
              $  IrStructInit (BasicTypeStruct vtableName)
              $  [ (tpName $ functionName method, IrIdentifier $ methodName)
                 | ((methodName, _), method) <- zip methods (implMethods i)
                 ]
              ++ [ ( tpName $ functionName f
                   , IrIdentifier $ subPath (implName i) $ tpName $ functionName
                     f
                   )
                 | f <- implStaticMethods i
                 ]
            }
        staticMethods <- forM
          (implStaticMethods i)
          (\method -> generateDeclIr ctx mod $ functionDecl $ method
            { functionName = subPath (implName i) $ tpName $ functionName method
            }
          )

        methodBundles <- forM (implMethods i)
          $ \x -> generateDeclIr ctx mod $ functionDecl x

        return
          [ foldr
                (\b acc -> mergeBundles acc b)
                (IrBundle
                  traitName
                  (  (map snd methods)
                  ++ [varDecl impl]
                  )
                )
              $ foldr (++) [] staticMethods
          ]

    _ -> return []
