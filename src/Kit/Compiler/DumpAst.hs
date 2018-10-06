module Kit.Compiler.DumpAst where

import Control.Monad
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Parser
import Kit.Str

dumpModuleContent :: CompileContext -> Module -> [TypedDecl] -> IO ()
dumpModuleContent ctx mod defs = do
  putStrLn $ show mod
  forM_ defs (dumpModuleDecl ctx mod 0)
  putStrLn ""

dumpModuleDecl :: CompileContext -> Module -> Int -> TypedDecl -> IO ()
dumpModuleDecl ctx mod indent decl = do
  let i = take (indent * 2) (repeat ' ')
  case decl of
    DeclFunction f -> do
      putStr
        $  i
        ++ "  "
        ++ (foldr (++)
                  ""
                  [ show modifier ++ " " | modifier <- functionModifiers f ]
           )
        ++ "function "
        ++ (s_unpack $ showTypePath $ functionName f)
        ++ ": "
      if null $ functionArgs f
        then putStr "() -> "
        else do
          putStrLn "("
          forM_
            (functionArgs f)
            (\arg -> do
              t <- dumpCt ctx (argType arg)
              putStrLn $ "      " ++ s_unpack (argName arg) ++ ": " ++ t
            )
          putStr $ i ++ "    ) -> "
      rt <- dumpCt ctx (functionType f)
      putStrLn rt
      case functionBody f of
        Just x -> do
          out <- dumpAst ctx (indent + 2) x
          putStrLn out
        _ -> return ()
      putStrLn ""

    DeclVar v -> do
      t <- dumpCt ctx (varType v)
      putStrLn
        $  i
        ++ "  var "
        ++ (s_unpack $ showTypePath $ varName v)
        ++ ": "
        ++ t
        ++ "\n"
      case varDefault v of
        Just x -> do
          out <- dumpAst ctx (indent + 2) x
          putStrLn out
        _ -> return ()

    DeclTrait t -> do
      putStrLn $ i ++ "  trait " ++ (s_unpack $ showTypePath $ traitName t)
      forM_
        (traitMethods t)
        (\method -> dumpModuleDecl ctx mod (indent + 1) (DeclFunction method))

    DeclImpl t -> do
      traitName <- dumpCt ctx $ implTrait t
      forName   <- dumpCt ctx $ implFor t
      putStrLn $ i ++ "  impl " ++ traitName ++ " for " ++ forName
      forM_
        (implMethods t)
        (\method -> dumpModuleDecl ctx mod (indent + 1) (DeclFunction method))

    DeclType t -> do
      putStrLn $ i ++ "  type " ++ (s_unpack $ showTypePath $ typeName t)
      forM_
        (typeStaticMethods t)
        (\method -> dumpModuleDecl ctx mod (indent + 1) (DeclFunction method))
      forM_
        (typeStaticFields t)
        (\v -> do
          t <- dumpCt ctx (varType v)
          putStrLn
            $  "    static var "
            ++ (s_unpack $ showTypePath $ varName v)
            ++ ": "
            ++ t
          case varDefault v of
            Just x -> do
              out <- dumpAst ctx (indent + 3) x
              putStrLn out
            _ -> return ()
        )
      forM_
        (typeMethods t)
        (\method -> dumpModuleDecl ctx mod (indent + 1) (DeclFunction method))

      -- TODO: instance methods
      case typeSubtype t of
        Struct { structFields = f } -> do
          forM_
            f
            (\v -> do
              t <- dumpCt ctx (varType v)
              putStrLn
                $  "    var "
                ++ (s_unpack $ showTypePath $ varName v)
                ++ ": "
                ++ t
              case varDefault v of
                Just x -> do
                  out <- dumpAst ctx 3 x
                  putStrLn out
                _ -> return ()
            )
        Union { unionFields = f } -> do
          forM_
            f
            (\v -> do
              t <- dumpCt ctx (varType v)
              putStrLn
                $  "    var "
                ++ (s_unpack $ showTypePath $ varName v)
                ++ ": "
                ++ t
              case varDefault v of
                Just x -> do
                  out <- dumpAst ctx 3 x
                  putStrLn out
                _ -> return ()
            )
        _ -> return ()
      putStrLn ""

    _ -> return ()

dumpAst :: CompileContext -> Int -> TypedExpr -> IO String
dumpAst ctx indent e@(TypedExpr { tExpr = texpr, inferredType = t, tPos = pos })
  = do
    let dumpChild = dumpAst ctx (indent + 1)
    t' <- dumpCt ctx t
    let
      indented =
        (++)
          (  (take (indent * 2) $ repeat ' ')
          ++ (show $ (pos { file = "" }))
          ++ ": "
          )
    let f x =
          indented
            $  (if tIsLocalPtr e
                 then "LOCAL PTR: "
                 else if tIsLocal e then "LOCAL: " else ""
               )
            ++ x
            ++ ": "
            ++ t'
    let i x children =
          (do
            children' <- mapM dumpChild children
            return $ intercalate "\n" $ (f x) : children'
          )
    result <- case texpr of
      Block x           -> i "{}" x
      Literal v _       -> return $ f $ show v
      This              -> return $ f "this"
      Self              -> return $ f "Self"
      Identifier x      -> return $ f $ "identifier " ++ show x
      PreUnop  op a     -> i ("pre " ++ show op) [a]
      PostUnop op a     -> i ("post " ++ show op) [a]
      Binop op a b      -> i ("binary " ++ show op) [a, b]
      For   a  b c      -> i ("for") [a, b, c]
      While a  b False  -> i ("while") [a, b]
      While a  b True   -> i ("do") [a, b]
      If    a  b c      -> i ("if") (catMaybes [Just a, Just b, c])
      Continue          -> return $ f "continue"
      Break             -> return $ f "break"
      Return x          -> i "return" (catMaybes [x])
      -- Throw a
      Match x cases def -> i
        "match"
        (x : (foldr (++) [] [ [matchPattern c, matchBody c] | c <- cases ]))
      InlineCall a          -> i "inline" [a]
      Method tp params name -> i ("method " ++ s_unpack name ++ " of " ++ s_unpack (showTypePath tp) ++ show params) []
      Field a id            -> i ("field " ++ show id) [a]
      StructInit (TypeInstance tp _) fields ->
        i ("struct " ++ s_unpack (showTypePath tp)) (map snd fields)
      EnumInit _ constructor fields ->
        i ("enum " ++ (s_unpack $ showTypePath constructor)) (map snd fields)
      ArrayAccess a b       -> i "array access" [a, b]
      Call        a args    -> i "call" (a : args)
      Cast        a _       -> i "cast" [a]
      -- TokenExpr [TokenClass]
      Unsafe       a        -> i "unsafe" [a]
      BlockComment _        -> return $ f "/** ... */"
      -- LexMacro Str [TokenClass]
      RangeLiteral a b      -> i "_ ... _" [a, b]
      ArrayLiteral x        -> i "[]" x
      VarDeclaration id _ const a -> i ((if const then "const " else "var ") ++ show id) (catMaybes [a])
      Using u x             -> i ("using " ++ show u) [x]
      TupleInit slots       -> i "tuple" slots
      TupleSlot   x n       -> i ("tuple." ++ show n) [x]
      Box         _ x       -> i "box" [x]
      BoxedVtable _ x       -> i "box vtable" [x]
      BoxedValue  _ x       -> i "box value" [x]
      _                     -> return $ f $ "??? " ++ show texpr

    return $ result

dumpCt :: CompileContext -> ConcreteType -> IO String
dumpCt ctx t = case t of
  TypeInstance tp params -> do
    p <- mapM (dumpCt ctx) params
    return
      $  s_unpack (showTypePath tp)
      ++ (if null params then "" else "[" ++ intercalate ", " p ++ "]")
  TypeEnumConstructor tp d _ params -> do
    p <- mapM (dumpCt ctx) params
    return
      $  "enum constructor "
      ++ s_unpack (showTypePath d)
      ++ " of "
      ++ s_unpack (showTypePath tp)
      ++ "["
      ++ intercalate ", " p
      ++ "]"
  TypeTypeVar i -> do
    info <- getTypeVar ctx i
    let
      tv =
        "type var #"
          ++ show (typeVarId info)
          ++ (if null $ typeVarConstraints info
               then ""
               else ": " ++ intercalate
                 ", "
                 ( map (s_unpack . showTypePath . fst . fst)
                 $ typeVarConstraints info
                 )
             )
    case typeVarValue info of
      Just t -> if (ctxVerbose ctx > 1)
        then do
          x <- dumpCt ctx t
          return $ tv ++ " := " ++ x
        else dumpCt ctx t
      Nothing -> return tv
  TypeTuple t -> do
    parts <- forM t $ dumpCt ctx
    return $ "(" ++ intercalate ", " parts ++ ")"
  TypeFunction rt args var params -> do
    args <- forM args $ \(name, t) -> do
      t' <- dumpCt ctx t
      return (name, t')
    rt <- dumpCt ctx rt
    return
      $  "("
      ++ intercalate ", " [ s_unpack name ++ ": " ++ t | (name, t) <- args ]
      ++ ") -> "
      ++ rt
  TypePtr t -> do
    t' <- dumpCt ctx t
    return $ "Ptr[" ++ t' ++ "]"
  _ -> return $ show t

