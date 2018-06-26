{
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Kit.Parser.Lexer where

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
  "?" { tok Question }

  -- comments
  "/**" ([^\*]|\*[^\/]|\*\n|\n)* "*/" { tok' (\s -> DocComment $ s_dropWhile whitespace $ s_take (s_length s - 5) $ s_drop 3 s) }
  "/*" ([^\*]|\*[^\/]|\*\n|\n)* "*/";
  "//" [^\n]*;

  -- keywords
  abstract { tok KeywordAbstract }
  as { tok KeywordAs }
  atom { tok KeywordAtom }
  break { tok KeywordBreak }
  case { tok KeywordCase }
  code { tok KeywordCode }
  continue { tok KeywordContinue }
  copy { tok KeywordCopy }
  default { tok KeywordDefault }
  delete { tok KeywordDelete }
  do { tok KeywordDo }
  else { tok KeywordElse }
  enum { tok KeywordEnum }
  for { tok KeywordFor }
  function { tok KeywordFunction }
  if { tok KeywordIf }
  implement { tok KeywordImplement }
  import { tok KeywordImport }
  include { tok KeywordInclude }
  inline { tok KeywordInline }
  in { tok KeywordIn }
  macro { tok KeywordMacro }
  match { tok KeywordMatch }
  move { tok KeywordMove }
  new { tok KeywordNew }
  op { tok KeywordOp }
  override { tok KeywordOverride }
  private { tok KeywordPrivate }
  public { tok KeywordPublic }
  return { tok KeywordReturn }
  rule { tok KeywordRule }
  rules { tok KeywordRules }
  Self { tok KeywordSelf }
  static { tok KeywordStatic }
  struct { tok KeywordStruct }
  super { tok KeywordSuper }
  switch { tok KeywordSwitch }
  then { tok KeywordThen }
  this { tok KeywordThis }
  throw { tok KeywordThrow }
  token { tok KeywordToken }
  tokens { tok KeywordTokens }
  trait { tok KeywordTrait }
  unsafe { tok KeywordUnsafe }
  var { tok KeywordVar }
  while { tok KeywordWhile }

  -- literals
  "true" { tok $ LiteralBool True }
  "false" { tok $ LiteralBool False }
  [\"] [^\"]* [\"] { tok' (\s -> LiteralString $ s_take (s_length s - 2) $ s_drop 1 s) }
  "'" [^\']* "'" { tok' (\s -> LiteralString $ s_take (s_length s - 2) $ s_drop 1 s) }
  [\"]{3} ([^\"]|\"[^\"]|\"\"[^\"]|\n)* [\"]{3} { tok' (\s -> LiteralString $ s_take (s_length s - 6) $ s_drop 3 s) }
  \-?[0-9]+ "." [0-9]* { tok' (\s -> LiteralFloat s) }
  "0x" [0-9a-f]+ { tok' (\s -> LiteralInt s) }
  "0b" [01]+ { tok' (\s -> LiteralInt s) }
  "0o" [0-7]+ { tok' (\s -> LiteralInt s) }
  \-?(0|[1-9][0-9]*) { tok' (\s -> LiteralInt s) }

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
  [a-z_][a-zA-Z0-9_]* "!" { tok' (\s -> Lex $ s_take (s_length s - 1) s) }
  [a-z_][a-zA-Z0-9_]* { tokString LowerIdentifier }
  [A-Z][a-zA-Z0-9_]* { tokString UpperIdentifier }
  "$" [a-z_][a-zA-Z0-9_]* { tok' (\s -> MacroIdentifier $ s_drop 1 s) }
  "${" [a-z_][a-zA-Z0-9_]* "}" { tok' (\s -> MacroIdentifier $ s_take (s_length s - 3) $ s_drop 2 s) }

{
-- determine the end of a span containing this ByteString and beginning at (line, col)
posnOffset :: (Int, Int) -> Str -> (Int, Int)
posnOffset (line, col) s = (line + length indices, if length indices == 0 then col + l - 1 else (l - 1 - (fromIntegral $ last indices)))
                           where indices = s_findIndices ((==) '\n') s
                                 l = (fromIntegral $ s_length s)

-- wrapper to convert an Alex position to a Span
pos2span (AlexPn _ line col) s = Span {start_line = line, start_col = col, end_line = end_line, end_col = end_col} where (end_line, end_col) = posnOffset (line, col) s

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

scanTokens str = go (alexStartPos,'\n',str,0)
  where go inp@(pos,_,str,n) =
          case alexScan inp 0 of
            AlexEOF -> []
            AlexError ((AlexPn _ line column),_,r,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ ": " ++ (show r)
            AlexSkip  inp' len     -> go inp'
            AlexToken inp'@(_,_,_,n') _ act ->
              act pos (ByteString.take (n'-n) str) : go inp'
}
