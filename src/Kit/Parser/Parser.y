{
module Kit.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as B
import Kit.Ast.Expr
import Kit.Ast.Modifier
import Kit.Ast.Operator
import Kit.Ast.Span
import Kit.Ast.Type
import Kit.Ast.Value
import Kit.Parser.Lexer
import Kit.Parser.Token
}

%name parseTokens Expressions
%name parseExpr Expr
%name parseStatement Statement
%tokentype {Token}
%error {parseError}

%token
  "#[" {(MetaOpen,_)}
  '(' {(ParenOpen,_)}
  ')' {(ParenClose,_)}
  '{' {(CurlyBraceOpen,_)}
  '}' {(CurlyBraceClose,_)}
  '[' {(SquareBraceOpen,_)}
  ']' {(SquareBraceClose,_)}
  ',' {(Comma,_)}
  ':' {(Colon,_)}
  ';' {(Semicolon,_)}
  "..." {(TripleDot,_)}
  '.' {(Dot,_)}
  '#' {(Hash,_)}
  '$' {(Dollar,_)}
  "=>" {(Arrow,_)}
  '?' {(Question,_)}
  abstract {(KeywordAbstract,_)}
  as {(KeywordAs,_)}
  atom {(KeywordAtom,_)}
  break {(KeywordBreak,_)}
  case {(KeywordCase,_)}
  code {(KeywordCode,_)}
  continue {(KeywordContinue,_)}
  default {(KeywordDefault,_)}
  do {(KeywordDo,_)}
  else {(KeywordElse,_)}
  enum {(KeywordEnum,_)}
  for {(KeywordFor,_)}
  function {(KeywordFunction,_)}
  if {(KeywordIf,_)}
  implement {(KeywordImplement,_)}
  import {(KeywordImport,_)}
  inline {(KeywordInline,_)}
  in {(KeywordIn,_)}
  macro {(KeywordMacro,_)}
  match {(KeywordMatch,_)}
  new {(KeywordNew,_)}
  op {(KeywordOp,_)}
  override {(KeywordOverride,_)}
  private {(KeywordPrivate,_)}
  public {(KeywordPublic,_)}
  return {(KeywordReturn,_)}
  rule {(KeywordRule,_)}
  rules {(KeywordRules,_)}
  Self {(KeywordSelf,_)}
  struct {(KeywordStruct,_)}
  super {(KeywordSuper,_)}
  switch {(KeywordSwitch,_)}
  then {(KeywordThen,_)}
  this {(KeywordThis,_)}
  throw {(KeywordThrow,_)}
  token {(KeywordToken,_)}
  trait {(KeywordTrait,_)}
  unsafe {(KeywordUnsafe,_)}
  var {(KeywordVar,_)}
  while {(KeywordWhile,_)}
  doc_comment {(DocComment $$,_)}
  lex_macro {(Lex $$,_)}
  identifier {(LowerIdentifier $$,_)}
  upper_identifier {(UpperIdentifier $$,_)}
  "++" {(Op Inc,_)}
  "--" {(Op Dec,_)}
  '+' {(Op Add,_)}
  '-' {(Op Sub,_)}
  '*' {(Op Mul,_)}
  '/' {(Op Div,_)}
  '%' {(Op Mod,_)}
  "==" {(Op Eq,_)}
  "!=" {(Op Neq,_)}
  ">=" {(Op Gte,_)}
  "<=" {(Op Lte,_)}
  "<<" {(Op LeftShift,_)}
  ">>" {(Op RightShift,_)}
  '>' {(Op Gt,_)}
  '<' {(Op Lt,_)}
  "&&" {(Op And,_)}
  "||" {(Op Or,_)}
  '&' {(Op BitAnd,_)}
  '|' {(Op BitOr,_)}
  '^' {(Op BitXor,_)}
  '=' {(Op Assign,_)}
  assign_op {(Op (AssignOp $$),_)}
  '!' {(Op Invert,_)}
  '~' {(Op InvertBits,_)}
  "::" {(Op Cons,_)}
  custom_op {(Op (Custom $$),_)}

  bool {(LiteralBool $$, _)}
  str {(LiteralString $$, _)}
  float {(LiteralFloat $$, _)}
  int {(LiteralInt $$, _)}
%%

Expressions :: {[Expr]}
  : Expressions_ {reverse $1}
Expressions_ :: {[Expr]}
  : {[]}
  | Expressions ToplevelExpr {$2 : $1}

ToplevelExpr :: {Expr}
  : import ModulePath ';' {e $ Import (reverse $2)}
  | DocMetaMods atom upper_identifier ';' {
    e $ TypeDeclaration $ Structure {
      structure_name = $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = [],
      structure_type = Atom
    }
  }
  | DocMetaMods enum upper_identifier TypeParams '{' RewriteRulesOrVariants '}' {
    e $ TypeDeclaration $ Structure {
      structure_name = $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = reverse (snd $6),
      structure_type = Enum {
        enum_params = $4,
        enum_variants = reverse (fst $6)
      }
    }
  }
  | DocMetaMods struct upper_identifier TypeParams '{' RewriteRulesOrFields '}' {
    e $ TypeDeclaration $ Structure {
      structure_name = $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = reverse (snd $6),
      structure_type = Struct {
        struct_params = $4,
        struct_fields = reverse (fst $6)
      }
    }
  }
  | DocMetaMods abstract upper_identifier TypeParams ':' TypeSpec '{' RewriteRules '}' {
    e $ TypeDeclaration $ Structure {
      structure_name = $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = reverse $8,
      structure_type = Abstract {
        abstract_underlying_type = $6,
        abstract_params = $4
      }
    }
  }
  | DocMetaMods trait upper_identifier TypeParams '{' RewriteRules '}' {
    e $ TraitDeclaration $ TraitDefinition {
      trait_name = $3,
      trait_doc = doc $1,
      trait_meta = reverse $ metas $1,
      trait_modifiers = reverse $ mods $1,
      trait_params = $4,
      trait_rules = $6
    }
  }
  | DocComment implement TypeSpec for TypeSpec '{' RewriteRules '}' {
    e $ Implement $ TraitImplementation {
      impl_trait = $3,
      impl_for = $5,
      impl_rules = $7,
      impl_doc = $1
    }
  }
  | Statement {$1}

Statement :: {Expr}
  : StandaloneExpr {$1}
  | VarDefinition {e $ Var $1}
  | return Statement {e $ Return $2}
  | throw Statement {e $ Throw $2}
  | continue ';' {e $ Continue}
  | break ';' {e $ Break}
  | Expr ';' {$1}

Statements :: {[Expr]}
  : {[]}
  | Statements Statement {$2 : $1}

StandaloneExpr :: {Expr}
  : ExprBlock {$1}
  --| LexMacroExprNonRecursiveBlock {$1}
  | if BinopTermOr ExprBlock else ExprBlock {e $ If $2 $3 (Just $5)}
  | if BinopTermOr ExprBlock {e $ If $2 $3 (Nothing)}
  | for identifier in Expr ExprBlock {e $ For $2 $4 $5}
  | while Expr ExprBlock {e $ While $2 $3}
  | match Expr '{' MatchCases DefaultMatchCase '}' {e $ Match $2 (reverse $4) $5}
  | FunctionDecl {$1}

FunctionDecl :: {Expr}
  : DocMetaMods function identifier TypeParams '(' Args ')' TypeAnnotation OptionalBody {
    e $ FunctionDeclaration $ FunctionDefinition {
      function_name = $3,
      function_doc = doc $1,
      function_meta = reverse $ metas $1,
      function_modifiers = reverse $ mods $1,
      function_params = $4,
      function_type = $8,
      function_args = reverse $6,
      function_body = $9
    }
  }

MatchCases :: {[MatchCase]}
  : {[]}
  | MatchCases MatchCase {$2 : $1}

MatchCase :: {MatchCase}
  : Expr "=>" Statement {MatchCase {match_pattern = $1, match_body = $3}}

DefaultMatchCase :: {Maybe Expr}
  : {Nothing}
  | default "=>" Statement {Just $3}

ExprBlock :: {Expr}
  : '{' Statements '}' {e $ Block $ reverse $2}

ModulePath :: {[B.ByteString]}
  : {[]}
  | identifier {[$1]}
  | ModulePath '.' identifier {$3 : $1}

Metadata :: {Metadata}
  : "#[" identifier '(' CallArgs ')' ']' {Metadata {meta_name = $2, meta_args = reverse $4}}
  | "#[" identifier ']' {Metadata {meta_name = $2, meta_args = []}}

Metas :: {[Metadata]}
  : {[]}
  | Metas Metadata {$2 : $1}

CallArgs :: {[Expr]}
  : {[]}
  | Expr {[$1]}
  | CallArgs ',' Expr {$3 : $1}

  {-Toplevel_Expr: Expr {
    import[e] => e,
    atom_decl[e] => e,
    struct_decl[e] => e,
    abstract_decl[e] => e,
    trait_decl[e] => e,
    trait_impl[e] => e,
    enum_decl[e] => e,
    statement[e] => e,-}

DocComment :: {Maybe B.ByteString}
  : {Nothing}
  | doc_comment {Just $1}

Modifiers :: {[Modifier]}
  : {[]}
  | Modifiers public {Public : $1}
  | Modifiers private {Private : $1}
  | Modifiers macro {Macro : $1}
  | Modifiers inline {Inline : $1}
  | Modifiers override {Override : $1}

DocMetaMods :: {(Maybe B.ByteString, [Metadata], [Modifier])}
  : DocComment Metas Modifiers {($1, $2, $3)}

TypeAnnotation :: {Maybe TypeSpec}
  : {Nothing}
  | ':' TypeSpec {Just $2}

TypeSpec :: {TypeSpec}
  : TypePath TypeParams {ParameterizedTypePath ($1, reverse $2)}

OptionalTypeSpec :: {Maybe TypeSpec}
  : {Nothing}
  | TypeSpec {Just $1}

OptionalBody :: {Maybe Expr}
  : ';' {Nothing}
  | ExprBlock {Just $1}

TypeParams :: {[TypeParam]}
  : {[]}
  | '[' TypeParams_ ']' {reverse $2}

TypeParams_ :: {[TypeParam]}
  : TypeParam {[$1]}
  | TypeParams_ ',' TypeParam {$3 : $1}

TypeParam :: {TypeParam}
  : TypePath TypeParams {TypeParam {t = ($1, $2), constraints = []}}
  | TypePath TypeParams ':' TypeConstraints {TypeParam {t = ($1, $2), constraints = $4}}

TypeConstraints :: {[TypeSpec]}
  : TypeSpec {[$1]}
  | '(' ')' {[]}
  | '(' TypeConstraints_ ')' {reverse $2}

TypeConstraints_ :: {[TypeSpec]}
  : TypeSpec {[$1]}
  | TypeConstraints_ ',' TypeSpec {$3 : $1 }

TypePath :: {TypePath}
  : ModulePath '.' Self {($1, B.pack "Self")}
  | ModulePath '.' upper_identifier {($1, $3)}
  | upper_identifier {([], $1)}
  | Self {([], B.pack "Self")}

VarDefinition :: {VarDefinition}
  : DocMetaMods var identifier TypeAnnotation OptionalDefault ';' {
    VarDefinition {var_name = $3, var_doc = doc $1, var_meta = reverse (metas $1), var_type = $4, var_modifiers = reverse (mods $1), var_default = $5}
  }

OptionalDefault :: {Maybe Expr}
  :  {Nothing}
  | '=' Expr {Just $2}

EnumPrefix :: {((Maybe B.ByteString, [Metadata], [Modifier]), B.ByteString)}
  : DocMetaMods upper_identifier {($1, $2)}

EnumVariant :: {EnumVariant}
  : EnumPrefix ';' {
      EnumVariant {
        variant_name = snd $1,
        variant_doc = doc $ fst $1,
        variant_meta = reverse $ metas $ fst $1,
        variant_modifiers = reverse $ mods $ fst $1,
        variant_args = []
      }
    }
  | EnumPrefix '(' Args ')' ';' {
      EnumVariant {
        variant_name = snd $1,
        variant_doc = doc $ fst $1,
        variant_meta = reverse $ metas $ fst $1,
        variant_modifiers = reverse $ mods $ fst $1,
        variant_args = reverse $3
      }
    }

Args :: {[ArgSpec]}
  : {[]}
  | ArgSpec {[$1]}
  | Args ',' ArgSpec {$3 : $1}

ArgSpec  :: {ArgSpec}
  : identifier TypeAnnotation OptionalDefault {ArgSpec {arg_name = $1, arg_type = $2, arg_default = $3}}

RewriteRulesOrFields :: {([VarDefinition], [RewriteRule])}
  : {([], [])}
  | RewriteRulesOrFields RewriteRule {(fst $1, $2 : snd $1)}
  | RewriteRulesOrFields RuleBlock {(fst $1, $2 ++ snd $1)}
  | RewriteRulesOrFields VarDefinition {($2 : fst $1, snd $1)}

RewriteRulesOrVariants :: {([EnumVariant], [RewriteRule])}
  : {([], [])}
  | RewriteRulesOrVariants RewriteRule {(fst $1, $2 : snd $1)}
  | RewriteRulesOrVariants RuleBlock {(fst $1, $2 ++ snd $1)}
  | RewriteRulesOrVariants EnumVariant {($2 : fst $1, snd $1)}

RewriteRules :: {[RewriteRule]}
  : {[]}
  | RewriteRules RewriteRule {$2 : $1}
  | RewriteRules RuleBlock {$2 ++ $1}

RulePrefix :: {((Maybe B.ByteString, [Metadata], [Modifier]), [TypeParam])}
  : DocMetaMods rule TypeParams {($1, $3)}

RulesPrefix :: {((Maybe B.ByteString, [Metadata], [Modifier]), [TypeParam])}
  : DocMetaMods rules TypeParams {($1, $3)}

RewriteRule :: {RewriteRule}
  : RulePrefix Expr "=>" OptionalTypeSpec OptionalBody {
    RewriteRule {
      rule_doc = doc $ fst $1,
      rule_meta = reverse $ metas $ fst $1,
      rule_modifiers = reverse $ mods $ fst $1,
      rule_params = snd $1,
      rule_pattern = $2,
      rule_type = $4,
      rule_body = $5
    }
  }
  | RulePrefix Expr ';' {
    RewriteRule {
      rule_doc = doc $ fst $1,
      rule_meta = reverse $ metas $ fst $1,
      rule_modifiers = reverse $ mods $ fst $1,
      rule_params = snd $1,
      rule_pattern = $2,
      rule_type = Nothing,
      rule_body = Nothing
    }
  }

RuleBlock :: {[RewriteRule]}
  : RulesPrefix '{' ShortRules '}' {
    [RewriteRule {
      rule_doc = (case rule_doc of
        Just s -> rule_doc
        Nothing -> doc $ fst $1
      ),
      rule_meta = reverse (meta ++ (metas $ fst $1)),
      rule_modifiers = reverse (modifiers ++ (mods $ fst $1)),
      rule_params = snd $1,
      rule_pattern = pattern,
      rule_type = rule_type,
      rule_body = body
    } | (rule_doc, meta, modifiers, pattern, rule_type, body) <- $3]
  }

ShortRules :: {[(Maybe B.ByteString, [Metadata], [Modifier], Expr, Maybe TypeSpec, Maybe Expr)]}
  : {[]}
  | ShortRules ShortRule {$2 : $1}

ShortRule :: {(Maybe B.ByteString, [Metadata], [Modifier], Expr, Maybe TypeSpec, Maybe Expr)}
  : DocMetaMods Expr "=>" OptionalTypeSpec OptionalBody {
    (doc $1, metas $1, mods $1, $2, $4, $5)
  }
  | DocMetaMods Expr ';' {
    (doc $1, metas $1, mods $1, $2, Nothing, Nothing)
  }

Expr :: {Expr}
  : LexMacroExpr {$1}

LexMacroExpr :: {Expr}
  : {-LexMacroExprNonRecursiveBlock {$1}
  | LexMacroExprNonRecursive {$1}
  | -}RangeLiteral {$1}

{-LexMacroExprNonRecursive :: {Expr}
  : lex_macro LexMacroTokens {e $ LexMacro $1 (reverse $2)}

LexMacroExprNonRecursiveBlock :: {Expr}
  : lex_macro LexMacroTokens LexMacroTokenBlock {e $ LexMacro $1 ((reverse $2) ++ (reverse $3))}-}

RangeLiteral :: {Expr}
  : BinopTermAssign "..." BinopTermAssign {e $ RangeLiteral $1 $3}
  | BinopTermAssign {$1}

BinopTermAssign :: {Expr}
  : BinopTermCons {$1}
  | BinopTermAssign '=' BinopTermCons {e $ Binop Assign $1 $3}
  | BinopTermAssign assign_op BinopTermCons {e $ Binop (AssignOp $2) $1 $3}
BinopTermCons :: {Expr}
  : TokenExpr {$1}
  | TokenExpr "::" BinopTermCons {e $ Binop Cons $1 $3}
TokenExpr :: {Expr}
  : {-token LexMacroTokenAny {e $ TokenExpr $2}
  | -}BinopTermTernary {$1}
BinopTermTernary :: {Expr}
  : if BinopTermOr then BinopTermOr else BinopTermOr {e $ If $2 $4 (Just $6)}
  | BinopTermOr {$1}
BinopTermOr :: {Expr}
  : BinopTermAnd {$1}
  | BinopTermOr "||" BinopTermAnd {e $ Binop Or $1 $3}
BinopTermAnd :: {Expr}
  : BinopTermBitOr {$1}
  | BinopTermAnd "&&" BinopTermBitOr {e $ Binop And $1 $3}
BinopTermBitOr :: {Expr}
  : BinopTermBitXor {$1}
  | BinopTermBitOr '|' BinopTermBitXor {e $ Binop BitOr $1 $3}
BinopTermBitXor :: {Expr}
  : BinopTermBitAnd {$1}
  | BinopTermBitXor '^' BinopTermBitAnd {e $ Binop BitXor $1 $3}
BinopTermBitAnd :: {Expr}
  : BinopTermEq {$1}
  | BinopTermBitAnd '&' BinopTermEq {e $ Binop BitAnd $1 $3}
BinopTermEq :: {Expr}
  : BinopTermCompare {$1}
  | BinopTermEq "==" BinopTermCompare {e $ Binop Eq $1 $3}
  | BinopTermEq "!=" BinopTermCompare {e $ Binop Neq $1 $3}
BinopTermCompare :: {Expr}
  : BinopTermShift {$1}
  | BinopTermCompare '>' BinopTermShift {e $ Binop Gt $1 $3}
  | BinopTermCompare '<' BinopTermShift {e $ Binop Lt $1 $3}
  | BinopTermCompare ">=" BinopTermShift {e $ Binop Gte $1 $3}
  | BinopTermCompare "<=" BinopTermShift {e $ Binop Lte $1 $3}
BinopTermShift :: {Expr}
  : BinopTermAdd {$1}
  | BinopTermShift "<<" BinopTermAdd {e $ Binop LeftShift $1 $3}
  | BinopTermShift ">>" BinopTermAdd {e $ Binop RightShift $1 $3}
BinopTermAdd :: {Expr}
  : BinopTermMul {$1}
  | BinopTermAdd '+' BinopTermMul {e $ Binop Add $1 $3}
  | BinopTermAdd '-' BinopTermMul {e $ Binop Sub $1 $3}
BinopTermMul :: {Expr}
  : BinopTermCustom {$1}
  | BinopTermMul '*' BinopTermCustom {e $ Binop Mul $1 $3}
  | BinopTermMul '/' BinopTermCustom {e $ Binop Div $1 $3}
BinopTermCustom :: {Expr}
  : Unop {$1}
  | BinopTermCustom custom_op Unop {e $ Binop (Custom $2) $1 $3}

Unop :: {Expr}
  : "++" VecExpr {e $ PreUnop Inc $2}
  | "--" VecExpr {e $ PreUnop Dec $2}
  | '!' VecExpr {e $ PreUnop Invert $2}
  | '-' VecExpr {e $ PreUnop Sub $2}
  | '~' VecExpr {e $ PreUnop InvertBits $2}
  | '&' VecExpr {e $ PreUnop Ref $2}
  | '*' VecExpr {e $ PreUnop Deref $2}
  | VecExpr "++" {e $ PostUnop Inc $1}
  | VecExpr "--" {e $ PostUnop Dec $1}
  | VecExpr {$1}

VecExpr :: {Expr}
  : '[' ArrayElems ']' {e $ VectorLiteral $ reverse $2}
  | '[' ArrayElems ',' ']' {e $ VectorLiteral $ reverse $2}
  | '[' ']' { e $ VectorLiteral []}
  | CastExpr {$1}

CastExpr :: {Expr}
  : CastExpr as TypeSpec {e $ Cast $1 $3}
  | NewExpr {$1}

ArrayElems :: {[Expr]}
  : Expr {[$1]}
  | ArrayElems ',' Expr {$3 : $1}

NewExpr :: {Expr}
  : new TypeSpec '(' CallArgs ')' {e $ New $2 (reverse $4)}
  | ArrayAccessExpr {$1}

ArrayAccessExpr :: {Expr}
  : ArrayAccessExpr '[' Expr ']' {e $ ArrayAccess $1 $3}
  | CallExpr {$1}

CallExpr :: {Expr}
  : CallExpr '(' CallArgs ')' {e $ Call $1 (reverse $3)}
  | FieldExpr {$1}

FieldExpr :: {Expr}
  : FieldExpr '.' identifier {e $ Field $1 $3}
  | TypeAnnotatedExpr {$1}

TypeAnnotatedExpr :: {Expr}
  : TypeAnnotatedExpr ':' TypeSpec {e $ TypeAnnotation $1 $3}
  | BaseExpr {$1}

BaseExpr :: {Expr}
  : Term {e $ Literal $1}
  | this {e $ This}
  | identifier {e $ Identifier $1}
  | upper_identifier {e $ TypeConstructor $1}
  | Self {e Self}
  | '(' Expr ')' {$2}

Term :: {ValueLiteral}
  : bool {BoolValue $1}
  | str {StringValue $1}
  | int {IntValue $1}
  | float {FloatValue $1}

{

parseError :: [Token] -> a
parseError e = error $ show e

doc (a,_,_) = a
metas (_,b,_) = b
mods (_,_,c) = c

}
