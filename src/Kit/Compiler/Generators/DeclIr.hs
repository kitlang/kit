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

generateDeclIr :: CompileContext -> Module -> TypedDecl -> IO [IrDecl]
generateDeclIr ctx mod t = do
  let converter' = converter (typedToIr ctx mod)
                             (\pos -> findUnderlyingType ctx mod (Just pos))
  let paramConverter = \p -> converter'
  case t of
    DeclType def@(TypeDefinition { typeName = name }) -> do
      debugLog ctx $ "generating IR for " ++ s_unpack name ++ " in " ++ show mod
      -- TODO: params
      converted    <- convertTypeDefinition paramConverter def
      -- TODO: add declarations for instance methods
      staticFields <- forM
        (typeStaticFields def)
        (\field -> generateDeclIr ctx mod
          $ DeclVar (field { varNamespace = (modPath mod) ++ [name] })
        )
      staticMethods <- forM
        (typeStaticMethods def)
        (\method -> generateDeclIr ctx mod $ DeclFunction
          (method { functionNamespace = (modPath mod) ++ [name] })
        )
      instanceMethods <- forM
        (typeMethods def)
        (\method -> generateDeclIr ctx mod $ DeclFunction
          (method { functionNamespace = (modPath mod) ++ [name] })
        )
      subtype <- case typeSubtype converted of
        t@(Enum { enumVariants = variants }) -> do
          let newName n = if modIsCModule mod
                then n
                else mangleName (modPath mod ++ [typeName converted]) n
          let variants' =
                [ variant { variantName = newName $ variantName variant }
                | variant <- variants
                ]
          return $ t { enumVariants = variants' }
        x -> return x
      return
        $ (DeclType $ converted { typeSubtype = subtype })
        : (foldr (++) [] (staticFields ++ staticMethods ++ instanceMethods))

    DeclFunction f@(FunctionDefinition { functionName = name }) -> do
      debugLog ctx
        $  "generating IR for function "
        ++ s_unpack name
        ++ " in "
        ++ show mod

      let isMain =
            (functionName f == "main")
              && (ctxMainModule ctx == modPath mod)
              && not (ctxIsLibrary ctx)

      converted <- convertFunctionDefinition paramConverter f

      if (isMain && functionType converted == BasicTypeVoid)
        then return
          [ DeclFunction $ converted
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
                    (IrReturn $ Just $ IrLiteral $ IntValue 0 $ BasicTypeCInt
                    )
              }
          ]
        else return
          [ DeclFunction $ converted
              { functionName = mangleName (functionNamespace f) name
              , functionType = if isMain
                then BasicTypeCInt
                else functionType converted
              }
          ]

    DeclVar v@(VarDefinition { varName = name }) -> do
      debugLog ctx
        $  "generating IR for var "
        ++ s_unpack name
        ++ " in "
        ++ show mod

      converted <- convertVarDefinition converter' v
      return
        [DeclVar $ converted { varName = mangleName (varNamespace v) name }]

    DeclTrait (      TraitDefinition { traitMethods = [] }) -> return []
    DeclTrait trait@(TraitDefinition { traitName = name } ) -> do
      -- FIXME: params
      converted <- convertTraitDefinition paramConverter trait
      -- trait declarations become struct definitions for the box/vtable
      let boxName    = mangleName ((modPath mod) ++ [name]) "box"
      let vtableName = mangleName ((modPath mod) ++ [name]) "vtable"
      let
        traitBox = newTypeDefinition
          { typeName    = boxName
          , typeSubtype = Struct
            { structFields = [ newVarDefinition { varName = valuePointerName
                                                , varType = CPtr BasicTypeVoid
                                                }
                             , newVarDefinition
                               { varName = "__vtable"
                               , varType = CPtr
                                 $ BasicTypeStruct (Just vtableName) []
                               }
                             ]
            }
          }
      let
        vtable = newTypeDefinition
          { typeName    = vtableName
          , typeSubtype = Struct
            { structFields = [ newVarDefinition
                                 { varName = functionName f
                                 , varType = CPtr $ BasicTypeFunction
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
      return [DeclType $ traitBox, DeclType $ vtable]

    DeclImpl (TraitImplementation { implMethods = [] }) -> return []
    DeclImpl i@(TraitImplementation { implTrait = TypeTraitConstraint ((mp, name), params), implFor = ct, implMod = implMod })
      -> do
      -- FIXME: for now we're indexing trait implementations by basic type, but
      -- different concrete types of the same basic type could each have their own
        for <- findUnderlyingType ctx mod (Just $ implPos i) ct
        let implName =
              (mangleName (mp ++ [name, "impl"] ++ implMod)
                          (s_pack $ basicTypeAbbreviation for)
              )
        let vtableName = mangleName ((modPath mod) ++ [name]) "vtable"
        methods <- forM (implMethods i) $ \method -> do
          f' <- convertFunctionDefinition paramConverter method
          let f = implicitifyMethod (CPtr BasicTypeVoid) vThisArgName f'
          let name' =
                (mangleName
                  (  mp
                  ++ [name, "impl"]
                  ++ implMod
                  ++ [s_pack $ basicTypeAbbreviation for]
                  )
                  (functionName f)
                )
          return
            ( name'
            , DeclFunction $ f
              { functionName = name'
              , functionBody = let
                                 v = IrVarDeclaration
                                   thisArgName
                                   for
                                   (Just $ IrPreUnop
                                     Deref
                                     (IrCast (IrIdentifier vThisArgName) (CPtr for))
                                   )
                               in  case functionBody f of
                                     Just (IrBlock x) -> Just (IrBlock (v : x))
                                     Just x           -> Just $ IrBlock [v, x]
                                     _                -> Nothing
              }
            )
        let impl = newVarDefinition
              { varName    = implName
              , varType    = BasicTypeStruct (Just vtableName) []
              , varDefault = Just $ IrStructInit
                (BasicTypeStruct (Just vtableName) [])
                [ (functionName method, IrIdentifier mangledName)
                | ((mangledName, _), method) <- zip methods (implMethods i)
                ]
              }
        return $ (map snd methods) ++ [DeclVar $ impl]

    _ -> return [] -- TODO
