{
{-# LANGUAGE OverloadedStrings                  #-}

module Kit.Parser.Lexer where

import Data.Char
import Numeric
import System.Exit
import Kit.Str
import Kit.Ast.Operator
import Kit.Parser.Span
import Kit.Parser.Token
}

%wrapper "posn-bytestring"

tokens :-

  -- syntactic elements
  $white+;
  "#[" { tok MetaOpen }
  "(" { tok ParenOpen }
  ")" { tok ParenClose }
  "{" { tok CurlyBraceOpen }
  "}" { tok CurlyBraceClose }
  "[" { tok SquareBraceOpen }
  "]" { tok SquareBraceClose }
  "," { tok Comma }
  ":" { tok Colon }
  ";" { tok Semicolon }
  "..." { tok TripleDot }
  "." { tok Dot }
  "#" { tok Hash }
  "$" { tok Dollar }
  "=>" { tok Arrow }
  "->" { tok FunctionArrow }
  "?" { tok Question }
  ".*" { tok WildcardSuffix }

  -- comments
  "/**" ([^\*]|\*[^\/]|\*\n|\n)* "*/" { tok' (\s -> DocComment $ s_dropWhile whitespace $ s_take (s_length s - 5) $ s_drop 3 s) }
  "/*" ([^\*]|\*[^\/]|\*\n|\n)* "*/";
  "//" [^\n]*;

  -- keywords
  abstract { tok KeywordAbstract }
  as { tok KeywordAs }
  atom { tok KeywordAtom }
  break { tok KeywordBreak }
  const { tok KeywordConst }
  continue { tok KeywordContinue }
  default { tok KeywordDefault }
  defer { tok KeywordDefer }
  do { tok KeywordDo }
  else { tok KeywordElse }
  empty { tok KeywordEmpty }
  enum { tok KeywordEnum }
  for { tok KeywordFor }
  function { tok KeywordFunction }
  if { tok KeywordIf }
  implement { tok KeywordImplement }
  implicit { tok KeywordImplicit }
  import { tok KeywordImport }
  include { tok KeywordInclude }
  inline { tok KeywordInline }
  in { tok KeywordIn }
  match { tok KeywordMatch }
  null { tok KeywordNull }
  private { tok KeywordPrivate }
  public { tok KeywordPublic }
  return { tok KeywordReturn }
  rule { tok KeywordRule }
  rules { tok KeywordRules }
  Self { tok KeywordSelf }
  sizeof { tok KeywordSizeof }
  specialise { tok KeywordSpecialize } -- common typo in the UK
  specialize { tok KeywordSpecialize }
  static { tok KeywordStatic }
  struct { tok KeywordStruct }
  then { tok KeywordThen }
  this { tok KeywordThis }
  throw { tok KeywordThrow }
  trait { tok KeywordTrait }
  type { tok KeywordType }
  typedef { tok KeywordTypedef }
  union { tok KeywordUnion }
  unsafe { tok KeywordUnsafe }
  using { tok KeywordUsing }
  var { tok KeywordVar }
  while { tok KeywordWhile }

  -- literals
  "true" { tok $ LiteralBool True }
  "false" { tok $ LiteralBool False }
  [\"] (\\.|[^\"\\])* [\"] { tok' $ \s -> LiteralString $ processStringLiteral $ s_take (s_length s - 2) $ s_drop 1 s }
  "'" (\\.|[^\'\\])* "'" { tok' $ \s -> LiteralString $ processStringLiteral $ s_take (s_length s - 2) $ s_drop 1 s }
  [\"]{3} ([^\"]|\"[^\"]|\"\"[^\"]|\n)* [\"]{3} { tok' $ \s -> LiteralString $ processStringLiteral $ s_take (s_length s - 6) $ s_drop 3 s }
  "c'" [.] "'" { tok' $ \s -> LiteralChar $ ord $ s_head $ s_drop 2 s }

  \-?[0-9]+ "." [0-9]* "_" (f(32|64)) { tok' (\s -> let [p1, p2] = s_split '_' s in LiteralFloat p1 (Just $ parseNumSuffix $ s_unpack p2)) }
  "0x" [0-9a-fA-F]+ "_" ([ui](8|16|32|64)|f(32|64)|[cis]) { tok' (\s -> let [p1, p2] = map s_unpack (s_split '_' s) in LiteralInt (parseInt readHex $ drop 2 $ p1) (Just $ parseNumSuffix p2)) }
  "0o" [0-7]+ "_" ([ui](8|16|32|64)|f(32|64)|[cis]) { tok' (\s -> let [p1, p2] = map s_unpack (s_split '_' s) in LiteralInt (parseInt readOct $ drop 2 $ p1) (Just $ parseNumSuffix p2)) }
  "0b" [01]+ "_" ([ui](8|16|32|64)|f(32|64)|[cis]) { tok' (\s -> let [p1, p2] = map s_unpack (s_split '_' s) in LiteralInt (parseInt readBin $ drop 2 $ p1) (Just $ parseNumSuffix p2)) }
  \-?(0|[1-9][0-9]*) "_" ([ui](8|16|32|64)|f(32|64)|[cis]) { tok' (\s -> let [p1, p2] = map s_unpack (s_split '_' s) in LiteralInt (parseInt readDec $ p1) (Just $ parseNumSuffix p2)) }

  \-?[0-9]+ "." [0-9]* { tok' (\s -> LiteralFloat s Nothing) }
  "0x" [0-9a-fA-F]+ { tok' (\s -> LiteralInt (parseInt readHex $ drop 2 $ s_unpack s) Nothing) }
  "0o" [0-7]+ { tok' (\s -> LiteralInt (parseInt readOct $ drop 2 $ s_unpack s) Nothing) }
  "0b" [01]+ { tok' (\s -> LiteralInt (parseInt readBin $ drop 2 $ s_unpack s) Nothing) }
  \-?(0|[1-9][0-9]*) { tok' (\s -> LiteralInt (parseInt readDec $ s_unpack s) Nothing) }

  -- operators
  "+=" { tok $ Op $ AssignOp Add }
  "-=" { tok $ Op $ AssignOp Sub }
  "/=" { tok $ Op $ AssignOp Div }
  "*=" { tok $ Op $ AssignOp Mul }
  "%=" { tok $ Op $ AssignOp Mod }
  "&&=" { tok $ Op $ AssignOp And }
  "||=" { tok $ Op $ AssignOp Or }
  "&=" { tok $ Op $ AssignOp BitAnd }
  "|=" { tok $ Op $ AssignOp BitOr }
  "^=" { tok $ Op $ AssignOp BitXor }
  "<<=" { tok $ Op $ AssignOp LeftShift }
  ">>=" { tok $ Op $ AssignOp RightShift }
  "=" { tok $ Op $ Assign }
  "++" { tok $ Op Inc }
  "--" { tok $ Op Dec }
  "+" { tok $ Op Add }
  "-" { tok $ Op Sub }
  "/" { tok $ Op Div }
  "*" { tok $ Op Mul }
  "%" { tok $ Op Mod }
  "==" { tok $ Op Eq }
  "!=" { tok $ Op Neq }
  ">=" { tok $ Op Gte }
  "<=" { tok $ Op Lte }
  "<<" { tok $ Op LeftShift }
  ">>" { tok $ Op RightShift }
  ">" { tok $ Op Gt }
  "<" { tok $ Op Lt }
  "&&" { tok $ Op And }
  "||" { tok $ Op Or }
  "&" { tok $ Op BitAnd }
  "|" { tok $ Op BitOr }
  "^" { tok $ Op BitXor }
  "!" { tok $ Op Invert }
  "~" { tok $ Op InvertBits }
  "::" { tok $ Op Cons }
  [\*\/\+\-\^\=\<\>\!\&\%\~\@\?\:\.]+ { tok' (\s -> Op $ Custom s) }

  -- identifiers
  [_]*[a-z][a-zA-Z0-9_]* "!" { tok' (\s -> Lex $ s_take (s_length s - 1) s) }
  [_]*[a-z][a-zA-Z0-9_]* { tokString LowerIdentifier }
  "`" [^`]+ "`" { tok' (\s -> LowerIdentifier $ s_take (s_length s - 2) $ s_drop 1 s) }
  [_]*[A-Z][a-zA-Z0-9_]* { tokString UpperIdentifier }
  "``" ([^`]|\`[^`])+ "``" { tok' (\s -> UpperIdentifier $ s_take (s_length s - 4) $ s_drop 2 s) }
  "$" [a-z_][a-zA-Z0-9_]* { tok' (\s -> MacroIdentifier $ s_drop 1 s) }
  "${" [a-z_][a-zA-Z0-9_]* "}" { tok' (\s -> MacroIdentifier $ s_take (s_length s - 3) $ s_drop 2 s) }

  "_" _+ { tokString LowerIdentifier }
  "_" { tok Underscore }

{
-- determine the end of a span containing this ByteString and beginning at (line, col)
posnOffset :: (Int, Int) -> Str -> (Int, Int)
posnOffset (line, col) s = (line + length indices, if length indices == 0 then col + l - 1 else (l - 1 - (fromIntegral $ last indices)))
                           where indices = s_findIndices ((==) '\n') s
                                 l = (fromIntegral $ s_length s)

-- wrapper to convert an Alex position to a Span
pos2span (AlexPn _ line col) s = (line, col, endLine, endCol) where (endLine, endCol) = posnOffset (line, col) s

processStringLiteral :: Str -> Str
processStringLiteral s = s_pack (_processString (s_unpack s) False) -- FIXME: better way?
_processString ('\\':t) False = _processString t True
_processString (h:t) False = h : (_processString t False)
_processString ('a':t) True = '\a' : (_processString t False)
_processString ('b':t) True = '\b' : (_processString t False)
_processString ('f':t) True = '\f' : (_processString t False)
_processString ('n':t) True = '\n' : (_processString t False)
_processString ('r':t) True = '\r' : (_processString t False)
_processString ('t':t) True = '\t' : (_processString t False)
_processString ('v':t) True = '\v' : (_processString t False)
_processString (h:t) True = h : (_processString t False)
_processString [] _ = ""

readBin = readInt 2 (\c -> c == '0' || c == '1') digitToInt
parseInt f ('-':s) = -(fst $ head $ f s)
parseInt f s = fst $ head $ f s
parseNumSuffix "c" = CChar
parseNumSuffix "i" = CInt
parseNumSuffix "s" = CSize
parseNumSuffix "u8" = Uint8
parseNumSuffix "u16" = Uint16
parseNumSuffix "u32" = Uint32
parseNumSuffix "u64" = Uint64
parseNumSuffix "i8" = Int8
parseNumSuffix "i16" = Int16
parseNumSuffix "i32" = Int32
parseNumSuffix "i64" = Int64
parseNumSuffix "f32" = Float32
parseNumSuffix "f64" = Float64

-- token helpers
tok' f p s = (f s, pos2span p s)
tok x = tok' (\s -> x)
tokString x = tok' (\s -> x s)
whitespace :: Char -> Bool
whitespace s = s == ' ' || s == '\n' || s == '\t' || s == '\r'

token_pos :: Token -> Span
token_pos (_, p) = p

token_type :: Token -> TokenClass
token_type (t, _) = t

scanTokens f str = go (alexStartPos,'\n',str,0)
  where go inp@(pos,_,str,n) =
          case alexScan inp 0 of
            AlexEOF -> []
            AlexError ((AlexPn _ line column),_,r,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ ": " ++ (show r)
            AlexSkip  inp' len     -> go inp'
            AlexToken inp'@(_,_,_,n') _ act ->
              (a, b) : go inp'
              where (a', (b1, b2, b3, b4)) = act pos (ByteString.take (n'-n) str)
                    (a, b) = (a', sp f b1 b2 b3 b4)
}
