module Kit.Compiler.Generators.DeclIr where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Generators.FindUnderlyingType
import Kit.Compiler.Generators.NameMangling
import Kit.Compiler.Generators.TypedExprToIr
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

generateDeclIr :: CompileContext -> Module -> TypedDecl -> IO [DeclBundle]
generateDeclIr ctx mod t = do
  let converter' = converter (typedToIr ctx mod)
                             (\pos -> findUnderlyingType ctx mod (Just pos))
  let paramConverter = \p -> converter'
  case t of
    DeclType def@(TypeDefinition { typeName = name }) -> do
      debugLog ctx $ "generating IR for type " ++ (s_unpack $ showTypePath name)
      converted    <- convertTypeDefinition paramConverter def
      staticFields <- forM (typeStaticFields def)
                           (\field -> generateDeclIr ctx mod $ DeclVar field)
      staticMethods <- forM
        (typeStaticMethods def)
        (\method -> generateDeclIr ctx mod $ DeclFunction method)
      instanceMethods <- forM
        (typeMethods def)
        (\method -> generateDeclIr ctx mod $ DeclFunction method)
      subtype <- case typeSubtype converted of
        t@(Enum { enumVariants = variants }) -> do
          let newName n = if modIsCModule mod
                then n
                else subPath (typeName converted) (tpName n)
          let variants' =
                [ variant { variantName = newName $ variantName variant }
                | variant <- variants
                ]
          return $ t { enumVariants = variants' }
        x -> return x

      deps <- case subtype of
        Struct { structFields = fields } ->
          return $ foldr (++) [] $ map ((typeDeps True) . varType) fields
        Enum { enumVariants = variants } -> do
          return $ foldr (++) [] $ map
            (\variant ->
              foldr (++) [] $ map ((typeDeps True) . argType) $ variantArgs
                variant
            )
            variants
        _ -> return []

      return
        $ [ foldr
              (\b acc -> mergeBundles acc b)
              (DeclBundle name
                          [DeclType $ converted { typeSubtype = subtype }]
                          deps
              )
              (foldr (++) [] $ staticFields ++ staticMethods ++ instanceMethods)
          ]

    DeclFunction f@(FunctionDefinition { functionName = name }) -> do
      debugLog ctx
        $  "generating IR for function "
        ++ (s_unpack $ showTypePath name)

      let isMain =
            (functionName f == ([], "main"))
              && (ctxMainModule ctx == modPath mod)
              && not (ctxIsLibrary ctx)

      converted <- convertFunctionDefinition paramConverter f

      let deps =
            (typeDeps True (functionType converted))
              ++ (foldr (++) [] $ map ((typeDeps True) . argType) $ functionArgs
                   converted
                 )
              ++ (case functionBody converted of
                   Just x  -> exprDeps x
                   Nothing -> []
                 )

      if (isMain && functionType converted == BasicTypeVoid)
      then
        return
          $ [ DeclBundle
                name
                ([ DeclFunction $ converted
                     { functionName = name
                     , functionType = BasicTypeCInt
                     , functionBody = case functionBody converted of
                       Just x ->
                         Just
                           $ IrBlock
                               [ x
                               , IrReturn
                               $ Just
                               $ IrLiteral
                               $ IntValue 0
                               $ BasicTypeCInt
                               ]
                       Nothing ->
                         Just
                           ( IrReturn
                           $ Just
                           $ IrLiteral
                           $ IntValue 0
                           $ BasicTypeCInt
                           )
                     }
                 ]
                )
                deps
            ]
      else
        return
          $ [ DeclBundle
                name
                [ DeclFunction $ converted
                    { functionType = if isMain
                      then BasicTypeCInt
                      else functionType converted
                    }
                ]
                deps
            ]

    DeclVar v@(VarDefinition { varName = name }) -> do
      debugLog ctx $ "generating IR for var " ++ (s_unpack $ showTypePath name)
      converted <- convertVarDefinition converter' v
      return $ [DeclBundle (varName converted) [DeclVar $ converted] []]

    DeclTrait (      TraitDefinition { traitMethods = [] }) -> return []
    DeclTrait trait@(TraitDefinition { traitName = name } ) -> do
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
                                   (functionVarargs f)
                                 }
                             | f <- traitMethods converted
                             ]
            }
          }

      let
        methodDeps = map
          (\method ->
            foldr (++) []
              $ ( (typeDeps False (functionType method))
                : (map ((typeDeps False) . argType) (functionArgs method))
                )
          )
          (traitMethods converted)
      let deps = foldr (++) [] methodDeps

      return $ [DeclBundle name [DeclType $ traitBox, DeclType $ vtable] deps]

    DeclImpl (TraitImplementation { implMethods = [] }) -> return []
    DeclImpl i'@(TraitImplementation { implTrait = TypeTraitConstraint (traitName, traitParams), implFor = ct })
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
          return (name', DeclFunction $ f { functionName = name' })
        let impl = newVarDefinition
              { varName    = implName i
              , varType    = BasicTypeStruct vtableName
              , varDefault = Just $ IrStructInit
                (BasicTypeStruct vtableName)
                [ (tpName $ functionName method, IrIdentifier $ methodName)
                | ((methodName, _), method) <- zip methods (implMethods i)
                ]
              }

        methodBundles <- forM (implMethods i)
          $ \x -> generateDeclIr ctx mod $ DeclFunction x
        let
          deps =
            foldr (++) [] $ map bundleDependencies $ foldr (++) [] methodBundles

        return
          $ [ DeclBundle (implName i) ((map snd methods) ++ [DeclVar $ impl])
              $  ((DefDependency name) : (typeDeps True for))
              ++ deps
            ]

    _ -> return []
