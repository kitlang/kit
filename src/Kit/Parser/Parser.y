{
module Kit.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as B
import Kit.Error
import Kit.Ast.Expr
import Kit.Ast.Modifier
import Kit.Ast.Operator
import Kit.Ast.Type
import Kit.Ast.Value
import Kit.Parser.Lexer
import Kit.Parser.Span
import Kit.Parser.Token
}

%name parseTokens Expressions
%name parseExpr Expr
%name parseStatement Statement
%tokentype {Token}
%error {parseError}

%monad { Parser } { thenP } { returnP }

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
  static {(KeywordStatic,_)}
  struct {(KeywordStruct,_)}
  super {(KeywordSuper,_)}
  switch {(KeywordSwitch,_)}
  then {(KeywordThen,_)}
  this {(KeywordThis,_)}
  throw {(KeywordThrow,_)}
  token {(KeywordToken,_)}
  tokens {(KeywordTokens,_)}
  trait {(KeywordTrait,_)}
  unsafe {(KeywordUnsafe,_)}
  var {(KeywordVar,_)}
  while {(KeywordWhile,_)}
  doc_comment {(DocComment _,_)}
  identifier {(LowerIdentifier _,_)}
  macro_identifier {(MacroIdentifier _,_)}
  upper_identifier {(UpperIdentifier _,_)}
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
  assign_op {(Op (AssignOp _),_)}
  '!' {(Op Invert,_)}
  '~' {(Op InvertBits,_)}
  "::" {(Op Cons,_)}
  custom_op {(Op (Custom _),_)}

  bool {(LiteralBool _, _)}
  str {(LiteralString _, _)}
  float {(LiteralFloat _, _)}
  int {(LiteralInt _, _)}
%%

Expressions :: {[Expr]}
  : Expressions_ {reverse $1}
Expressions_ :: {[Expr]}
  : {[]}
  | Expressions ToplevelExpr {$2 : $1}

ToplevelExpr :: {Expr}
  : import ModulePath ';' {pe (p $1 <+> p $3) $ Import (reverse $ fst $2)}
  | DocMetaMods atom upper_identifier ';' {
    pe (fp [p $1, p $2, p $4]) $ TypeDeclaration $ Structure {
      structure_name = extract_upper_identifier $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = [],
      structure_type = Atom
    }
  }
  | DocMetaMods enum upper_identifier TypeParams '{' RewriteRulesOrVariants '}' {
    pe (fp [p $1, p $2, p $7]) $ TypeDeclaration $ Structure {
      structure_name = extract_upper_identifier $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = reverse (snd $6),
      structure_type = Enum {
        enum_params = fst $4,
        enum_variants = reverse (fst $6)
      }
    }
  }
  | DocMetaMods struct upper_identifier TypeParams '{' RewriteRulesOrFields '}' {
    pe (fp [p $1, p $2, p $7]) $ TypeDeclaration $ Structure {
      structure_name = extract_upper_identifier $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = reverse (snd $6),
      structure_type = Struct {
        struct_params = fst $4,
        struct_fields = reverse (fst $6)
      }
    }
  }
  | DocMetaMods abstract upper_identifier TypeParams TypeAnnotation '{' RewriteRules '}' {
    pe (fp [p $1, p $2, p $8]) $ TypeDeclaration $ Structure {
      structure_name = extract_upper_identifier $3,
      structure_doc = doc $1,
      structure_meta = reverse $ metas $1,
      structure_modifiers = reverse $ mods $1,
      structure_rules = reverse $7,
      structure_type = Abstract {
        abstract_underlying_type = fst $5,
        abstract_params = fst $4
      }
    }
  }
  | DocMetaMods trait upper_identifier TypeParams '{' RewriteRules '}' {
    pe (fp [p $1, p $2, p $7]) $ TraitDeclaration $ TraitDefinition {
      trait_name = extract_upper_identifier $3,
      trait_doc = doc $1,
      trait_meta = reverse $ metas $1,
      trait_modifiers = reverse $ mods $1,
      trait_params = fst $4,
      trait_rules = $6
    }
  }
  | DocMetaMods implement TypeSpec for TypeSpec '{' RewriteRules '}' {
    pe (fp [p $1, p $2, p $8]) $ Implement $ TraitImplementation {
      impl_trait = fst $3,
      impl_for = fst $5,
      impl_rules = $7,
      impl_doc = doc $1
    }
  }
  | Statement {$1}

Statement :: {Expr}
  : StandaloneExpr {$1}
  | VarDefinition {pe (p $1) $ VarDef $ fst $1}
  | return Statement {pe (p $1 <+> pos $2) $ Return $ Just $2}
  | return ';' {pe (p $1 <+> p $2) $ Return $ Nothing}
  | throw Statement {pe (p $1 <+> pos $2) $ Throw $2}
  | continue ';' {pe (p $1 <+> p $2) $ Continue}
  | break ';' {pe (p $1 <+> p $2) $ Break}
  | tokens LexMacroTokenBlock {pe (snd $1 <+> snd $2) $ TokenExpr $ tc (fst $2)}
  | Expr ';' {me (pos $1 <+> p $2) $1}
  | macro_identifier {pe (p $1) (Lvalue $ MacroVar $ extract_macro_identifier $1)}

StandaloneExpr :: {Expr}
  : ExprBlock {$1}
  | if BinopTermOr ExprBlock else ExprBlock {pe (p $1 <+> pos $5) $ If $2 $3 (Just $5)}
  | if BinopTermOr ExprBlock {pe (p $1 <+> pos $3) $ If $2 $3 (Nothing)}
  | for Lvalue in Expr ExprBlock {pe (p $1 <+> pos $5) $ For (pe (snd $2) (Lvalue $ fst $2)) $4 $5}
  | while Expr ExprBlock {pe (p $1 <+> pos $3) $ While $2 $3}
  | match Expr '{' MatchCases DefaultMatchCase '}' {pe (p $1 <+> p $6) $ Match $2 (reverse $4) $5}
  | FunctionDecl {$1}

FunctionDecl :: {Expr}
  : DocMetaMods function identifier TypeParams '(' Args ')' FunctionTypeAnnotation OptionalBody {
    pe (fp [p $1, p $2, p $7, snd $8, snd $9]) $ FunctionDeclaration $ FunctionDefinition {
      function_name = extract_identifier $3,
      function_doc = doc $1,
      function_meta = reverse $ metas $1,
      function_modifiers = reverse $ mods $1,
      function_params = fst $4,
      function_type = fst $8,
      function_args = reverse $6,
      function_body = fst $9
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
  : '{' Statements '}' {pe (p $1 <+> p $3) $ Block $ reverse $2}

Statements :: {[Expr]}
  : {[]}
  | Statements Statement {$2 : $1}

ModulePath :: {([B.ByteString], Span)}
  : {([], null_span)}
  | identifier {([extract_identifier $1], p $1)}
  | ModulePath '.' identifier {(extract_identifier $3 : fst $1, snd $1 <+> p $3)}

Metadata :: {(Metadata, Span)}
  : "#[" identifier '(' CallArgs ')' ']' {(Metadata {meta_name = extract_identifier $2, meta_args = reverse $4}, (p $1) <+> (p $6))}
  | "#[" identifier ']' {(Metadata {meta_name = extract_identifier $2, meta_args = []}, (p $1) <+> (p $3))}

Metas :: {([Metadata], Span)}
  : {([], null_span)}
  | Metas Metadata {(fst $2 : fst $1, snd $1 <+> p $2)}

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
  | doc_comment {Just $ extract_doc_comment $1}

Modifiers :: {([Modifier], Span)}
  : {([], null_span)}
  | Modifiers public {(Public : fst $1, snd $1 <+> p $2)}
  | Modifiers private {(Private : fst $1, snd $1 <+> p $2)}
  | Modifiers macro {(Macro : fst $1, snd $1 <+> p $2)}
  | Modifiers inline {(Inline : fst $1, snd $1 <+> p $2)}
  | Modifiers override {(Override : fst $1, snd $1 <+> p $2)}
  | Modifiers static {(Static : fst $1, snd $1 <+> p $2)}

DocMetaMods :: {((Maybe B.ByteString, [Metadata], [Modifier]), Span)}
  : DocComment Metas Modifiers {(($1, fst $2, fst $3), (p $2) <+> (p $3))}

TypeAnnotation :: {(Maybe TypeSpec, Span)}
  : {(Nothing, null_span)}
  | ':' TypeSpec {(Just $ fst $2, p $1 <+> p $2)}

FunctionTypeAnnotation :: {(Maybe TypeSpec, Span)}
  : {(Nothing, null_span)}
  | "=>" TypeSpec {(Just $ fst $2, p $1 <+> p $2)}
  | "=>" {(Nothing, null_span)}

TypeSpec :: {(TypeSpec, Span)}
  : TypePath TypeParams {(ParameterizedTypePath (fst $1, reverse $ fst $2), p $1 <+> p $2)}

OptionalTypeSpec :: {(Maybe TypeSpec, Span)}
  : {(Nothing, null_span)}
  | TypeSpec {(Just $ fst $1, snd $1)}

OptionalBody :: {(Maybe Expr, Span)}
  : ';' {(Nothing, null_span)}
  | ExprBlock {(Just $1, pos $1)}

TypeParams :: {([TypeParam], Span)}
  : {([], null_span)}
  | '[' TypeParams_ ']' {(reverse $2, p $1 <+> p $3)}

TypeParams_ :: {[TypeParam]}
  : TypeParam {[$1]}
  | TypeParams_ ',' TypeParam {$3 : $1}

TypeParam :: {TypeParam}
  : TypePath TypeParams {TypeParam {t = (fst $1, fst $2), constraints = []}}
  | TypePath TypeParams ':' TypeConstraints {TypeParam {t = (fst $1, fst $2), constraints = $4}}

TypeConstraints :: {[TypeSpec]}
  : TypeSpec {[fst $1]}
  | '(' ')' {[]}
  | '(' TypeConstraints_ ')' {reverse $2}

TypeConstraints_ :: {[TypeSpec]}
  : TypeSpec {[fst $1]}
  | TypeConstraints_ ',' TypeSpec {fst $3 : $1}

TypePath :: {(TypePath, Span)}
  : ModulePath '.' Self {((fst $1, B.pack "Self"), p $1 <+> p $3)}
  | ModulePath '.' upper_identifier {((fst $1, extract_upper_identifier $3), p $1 <+> p $3)}
  | upper_identifier {(([], extract_upper_identifier $1), p $1)}
  | Self {(([], B.pack "Self"), p $1)}

VarDefinition :: {(VarDefinition, Span)}
  : DocMetaMods var Lvalue TypeAnnotation OptionalDefault ';' {
    (VarDefinition {
      var_name = fst $3,
      var_doc = doc $1,
      var_meta = reverse (metas $1),
      var_type = fst $4,
      var_modifiers = reverse (mods $1),
      var_default = $5}, fp [p $1, p $2, p $6])
  }

OptionalDefault :: {Maybe Expr}
  :  {Nothing}
  | '=' Expr {Just $2}

EnumPrefix :: {(DocMetaMod, B.ByteString)}
  : DocMetaMods upper_identifier {($1, extract_upper_identifier $2)}

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
  : identifier TypeAnnotation OptionalDefault {ArgSpec {arg_name = extract_identifier $1, arg_type = fst $2, arg_default = $3}}

RewriteRulesOrFields :: {([VarDefinition], [RewriteRule])}
  : {([], [])}
  | RewriteRulesOrFields RewriteRule {(fst $1, $2 : snd $1)}
  | RewriteRulesOrFields RuleBlock {(fst $1, $2 ++ snd $1)}
  | RewriteRulesOrFields VarDefinition {(fst $2 : fst $1, snd $1)}

RewriteRulesOrVariants :: {([EnumVariant], [RewriteRule])}
  : {([], [])}
  | RewriteRulesOrVariants RewriteRule {(fst $1, $2 : snd $1)}
  | RewriteRulesOrVariants RuleBlock {(fst $1, $2 ++ snd $1)}
  | RewriteRulesOrVariants EnumVariant {($2 : fst $1, snd $1)}

RewriteRules :: {[RewriteRule]}
  : {[]}
  | RewriteRules RewriteRule {$2 : $1}
  | RewriteRules RuleBlock {$2 ++ $1}

RulePrefix :: {(DocMetaMod, [TypeParam])}
  : DocMetaMods rule TypeParams {($1, fst $3)}

RulesPrefix :: {(DocMetaMod, [TypeParam])}
  : DocMetaMods rules TypeParams {($1, fst $3)}

RewriteExpr :: {Expr}
  : Expr {$1}
  | StandaloneExpr {$1}

RewriteRule :: {RewriteRule}
  : RulePrefix RewriteExpr "=>" OptionalTypeSpec OptionalBody {
    Rule TermRewriteRule {
      rule_doc = doc $ fst $1,
      rule_meta = reverse $ metas $ fst $1,
      rule_modifiers = reverse $ mods $ fst $1,
      rule_params = snd $1,
      rule_pattern = $2,
      rule_type = fst $4,
      rule_body = fst $5
    }
  }
  | RulePrefix RewriteExpr ';' {
    Rule TermRewriteRule {
      rule_doc = doc $ fst $1,
      rule_meta = reverse $ metas $ fst $1,
      rule_modifiers = reverse $ mods $ fst $1,
      rule_params = snd $1,
      rule_pattern = $2,
      rule_type = Nothing,
      rule_body = Nothing
    }
  }
  | FunctionDecl {
    case expr $1 of
      FunctionDeclaration f -> Function f
  }

RuleBlock :: {[RewriteRule]}
  : RulesPrefix '{' ShortRules '}' {
    [Rule TermRewriteRule {
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
  : DocMetaMods RewriteExpr "=>" OptionalTypeSpec OptionalBody {
    (doc $1, metas $1, mods $1, $2, fst $4, fst $5)
  }
  | DocMetaMods Expr ';' {
    (doc $1, metas $1, mods $1, $2, Nothing, Nothing)
  }

Expr :: {Expr}
  : RangeLiteral {$1}

RangeLiteral :: {Expr}
  : BinopTermAssign "..." BinopTermAssign {pe (pos $1 <+> pos $3) $ RangeLiteral $1 $3}
  | BinopTermAssign {$1}

BinopTermAssign :: {Expr}
  : BinopTermCons {$1}
  | BinopTermAssign '=' BinopTermCons {pe (pos $1 <+> pos $3) $ Binop Assign $1 $3}
  | BinopTermAssign assign_op BinopTermCons {pe (pos $1 <+> pos $3) $ Binop (AssignOp (extract_assign_op $2)) $1 $3}
BinopTermCons :: {Expr}
  : TokenExpr {$1}
  | TokenExpr "::" BinopTermCons {pe (pos $1 <+> pos $3) $ Binop Cons $1 $3}
TokenExpr :: {Expr}
  : token LexMacroTokenAny {pe (snd $1 <+> snd $2) $ TokenExpr $ tc [$2]}
  | BinopTermTernary {$1}
BinopTermTernary :: {Expr}
  : if BinopTermOr then BinopTermOr else BinopTermOr {pe (p $1 <+> pos $6) $ If $2 $4 (Just $6)}
  | BinopTermOr {$1}
BinopTermOr :: {Expr}
  : BinopTermAnd {$1}
  | BinopTermOr "||" BinopTermAnd {pe (pos $1 <+> pos $3) $ Binop Or $1 $3}
BinopTermAnd :: {Expr}
  : BinopTermBitOr {$1}
  | BinopTermAnd "&&" BinopTermBitOr {pe (pos $1 <+> pos $3) $ Binop And $1 $3}
BinopTermBitOr :: {Expr}
  : BinopTermBitXor {$1}
  | BinopTermBitOr '|' BinopTermBitXor {pe (pos $1 <+> pos $3) $ Binop BitOr $1 $3}
BinopTermBitXor :: {Expr}
  : BinopTermBitAnd {$1}
  | BinopTermBitXor '^' BinopTermBitAnd {pe (pos $1 <+> pos $3) $ Binop BitXor $1 $3}
BinopTermBitAnd :: {Expr}
  : BinopTermEq {$1}
  | BinopTermBitAnd '&' BinopTermEq {pe (pos $1 <+> pos $3) $ Binop BitAnd $1 $3}
BinopTermEq :: {Expr}
  : BinopTermCompare {$1}
  | BinopTermEq "==" BinopTermCompare {pe (pos $1 <+> pos $3) $ Binop Eq $1 $3}
  | BinopTermEq "!=" BinopTermCompare {pe (pos $1 <+> pos $3) $ Binop Neq $1 $3}
BinopTermCompare :: {Expr}
  : BinopTermShift {$1}
  | BinopTermCompare '>' BinopTermShift {pe (pos $1 <+> pos $3) $ Binop Gt $1 $3}
  | BinopTermCompare '<' BinopTermShift {pe (pos $1 <+> pos $3) $ Binop Lt $1 $3}
  | BinopTermCompare ">=" BinopTermShift {pe (pos $1 <+> pos $3) $ Binop Gte $1 $3}
  | BinopTermCompare "<=" BinopTermShift {pe (pos $1 <+> pos $3) $ Binop Lte $1 $3}
BinopTermShift :: {Expr}
  : BinopTermAdd {$1}
  | BinopTermShift "<<" BinopTermAdd {pe (pos $1 <+> pos $3) $ Binop LeftShift $1 $3}
  | BinopTermShift ">>" BinopTermAdd {pe (pos $1 <+> pos $3) $ Binop RightShift $1 $3}
BinopTermAdd :: {Expr}
  : BinopTermMul {$1}
  | BinopTermAdd '+' BinopTermMul {pe (pos $1 <+> pos $3) $ Binop Add $1 $3}
  | BinopTermAdd '-' BinopTermMul {pe (pos $1 <+> pos $3) $ Binop Sub $1 $3}
BinopTermMul :: {Expr}
  : BinopTermCustom {$1}
  | BinopTermMul '*' BinopTermCustom {pe (pos $1 <+> pos $3) $ Binop Mul $1 $3}
  | BinopTermMul '/' BinopTermCustom {pe (pos $1 <+> pos $3) $ Binop Div $1 $3}
BinopTermCustom :: {Expr}
  : Unop {$1}
  | BinopTermCustom custom_op Unop {pe (pos $1 <+> pos $3) $ Binop (Custom $ extract_custom_op $2) $1 $3}

Unop :: {Expr}
  : "++" VecExpr {pe (p $1 <+> pos $2) $ PreUnop Inc $2}
  | "--" VecExpr {pe (p $1 <+> pos $2) $ PreUnop Dec $2}
  | '!' VecExpr {pe (p $1 <+> pos $2) $ PreUnop Invert $2}
  | '-' VecExpr {pe (p $1 <+> pos $2) $ PreUnop Sub $2}
  | '~' VecExpr {pe (p $1 <+> pos $2) $ PreUnop InvertBits $2}
  | '&' VecExpr {pe (p $1 <+> pos $2) $ PreUnop Ref $2}
  | '*' VecExpr {pe (p $1 <+> pos $2) $ PreUnop Deref $2}
  | VecExpr "++" {pe (pos $1 <+> p $2) $ PostUnop Inc $1}
  | VecExpr "--" {pe (pos $1 <+> p $2) $ PostUnop Dec $1}
  | VecExpr {$1}

VecExpr :: {Expr}
  : '[' ArrayElems ']' {pe (p $1 <+> p $3) $ VectorLiteral $ reverse $2}
  | '[' ArrayElems ',' ']' {pe (p $1 <+> p $4) $ VectorLiteral $ reverse $2}
  | '[' ']' { pe (p $1 <+> p $2) $ VectorLiteral []}
  | CastExpr {$1}

CastExpr :: {Expr}
  : CastExpr as TypeSpec {pe (pos $1 <+> snd $3) $ Cast $1 $ fst $3}
  | NewExpr {$1}

ArrayElems :: {[Expr]}
  : Expr {[$1]}
  | ArrayElems ',' Expr {$3 : $1}

NewExpr :: {Expr}
  : new TypeSpec '(' CallArgs ')' {pe (p $1 <+> p $5) $ New (fst $2) (reverse $4)}
  | ArrayAccessExpr {$1}

ArrayAccessExpr :: {Expr}
  : ArrayAccessExpr '[' Expr ']' {pe (pos $1 <+> p $4) $ ArrayAccess $1 $3}
  | CallExpr {$1}

CallExpr :: {Expr}
  : CallExpr '(' CallArgs ')' {pe (pos $1 <+> p $4) $ Call $1 (reverse $3)}
  | FieldExpr {$1}

FieldExpr :: {Expr}
  : FieldExpr '.' identifier {pe (pos $1 <+> p $3) $ Field $1 $ extract_identifier $3}
  | TypeAnnotatedExpr {$1}

TypeAnnotatedExpr :: {Expr}
  : TypeAnnotatedExpr ':' TypeSpec {pe (pos $1 <+> snd $3) $ TypeAnnotation $1 (fst $3)}
  | BaseExpr {$1}

BaseExpr :: {Expr}
  : Term {pe (snd $1) (Literal $ fst $1)}
  | this {pe (snd $1) This}
  | Self {pe (snd $1) Self}
  | Lvalue {pe (snd $1) $ Lvalue $ fst $1}
  | upper_identifier {pe (snd $1) $ TypeConstructor $ extract_upper_identifier $1}
  | '(' Expr ')' {me (p $1 <+> p $3) $2}

Lvalue :: {(Lvalue, Span)}
  : identifier {(Var $ extract_identifier $1, snd $1)}
  | macro_identifier {(MacroVar $ extract_macro_identifier $1, snd $1)}

Term :: {(ValueLiteral, Span)}
  : bool {(BoolValue $ extract_bool $ fst $1, snd $1)}
  | str {(StringValue $ extract_lit $ fst $1, snd $1)}
  | int {(IntValue $ extract_lit $ fst $1, snd $1)}
  | float {(FloatValue $ extract_lit $ fst $1, snd $1)}

LexMacroTokenBlock :: {([Token], Span)}
  : '{' LexMacroTokensInsideBlock '}' {(reverse $2, snd $1 <+> snd $3)}

LexMacroTokensInsideBlock :: {[Token]}
  : {[]}
  | LexMacroTokensInsideBlock LexMacroToken {$2 : $1}
  | LexMacroTokensInsideBlock LexMacroTokenBlock {((CurlyBraceClose, null_span) : (reverse $ fst $2)) ++ ((CurlyBraceOpen, null_span) : $1)}

LexMacroTokenAny :: {Token}
  : '{' {$1}
  | '}' {$1}
  | LexMacroToken {$1}

LexMacroToken :: {Token}
  : '[' {$1}
  | ']' {$1}
  | '(' {$1}
  | ')' {$1}
  | ':' {$1}
  | "#[" {$1}
  | "..." {$1}
  | '.' {$1}
  | '#' {$1}
  | '$' {$1}
  | "=>" {$1}
  | '?' {$1}
  | ',' {$1}
  | ';' {$1}
  | abstract {$1}
  | as {$1}
  | atom {$1}
  | break {$1}
  | case {$1}
  | code {$1}
  | continue {$1}
  | default {$1}
  | do {$1}
  | else {$1}
  | enum {$1}
  | for {$1}
  | function {$1}
  | if {$1}
  | implement {$1}
  | import {$1}
  | inline {$1}
  | in {$1}
  | macro {$1}
  | match {$1}
  | new {$1}
  | op {$1}
  | override {$1}
  | private {$1}
  | public {$1}
  | return {$1}
  | rule {$1}
  | rules {$1}
  | Self {$1}
  | struct {$1}
  | super {$1}
  | switch {$1}
  | then {$1}
  | this {$1}
  | throw {$1}
  | token {$1}
  | tokens {$1}
  | trait {$1}
  | unsafe {$1}
  | var {$1}
  | while {$1}
  | doc_comment {$1}
  | identifier {$1}
  | macro_identifier {$1}
  | upper_identifier {$1}
  | "++" {$1}
  | "--" {$1}
  | '+' {$1}
  | '-' {$1}
  | '*' {$1}
  | '/' {$1}
  | '%' {$1}
  | "==" {$1}
  | "!=" {$1}
  | ">=" {$1}
  | "<=" {$1}
  | "<<" {$1}
  | ">>" {$1}
  | '>' {$1}
  | '<' {$1}
  | "&&" {$1}
  | "||" {$1}
  | '&' {$1}
  | '|' {$1}
  | '^' {$1}
  | '=' {$1}
  | assign_op {$1}
  | '!' {$1}
  | '~' {$1}
  | "::" {$1}
  | custom_op {$1}
  | bool {$1}
  | str {$1}
  | float {$1}
  | int {$1}
{

data Parser a = ParseResult a | Err Error

instance Functor Parser where
  fmap f (Err e) = Err e
  fmap f (ParseResult x) = ParseResult (f x)

instance Applicative Parser where
  pure = ParseResult
  (<*>) (ParseResult f) (ParseResult x) = ParseResult (f x)
  (<*>) (Err e) _ = Err e
  (<*>) _ (Err e) = Err e

instance Monad Parser where
  (>>=) m f = case m of
                Err e -> Err e
                ParseResult x -> f x
  return = ParseResult

thenP = (>>=)
returnP = return

parseError [] = Err $ err ParseError ("Unexpected end of input")
parseError t = Err $ errp ParseError ("Unexpected " ++ (show $ fst et)) (snd et) where et = t !! 0

-- projections
extract_identifier (LowerIdentifier x,_) = x
extract_macro_identifier (MacroIdentifier x,_) = x
extract_upper_identifier (UpperIdentifier x,_) = x
extract_bool (LiteralBool x) = x
extract_lit (LiteralInt x) = x
extract_lit (LiteralFloat x) = x
extract_lit (LiteralString x) = x
extract_assign_op (Op (AssignOp x),_) = x
extract_custom_op (Op (Custom x),_) = x
extract_doc_comment (DocComment x,_) = x

tc :: [Token] -> [TokenClass]
tc t = [fst t' | t' <- t]

p = snd

fp :: [Span] -> Span
fp s = foldr (<+>) null_span s

type DocMetaMod = ((Maybe B.ByteString, [Metadata], [Modifier]), Span)

doc :: ((Maybe B.ByteString, [Metadata], [Modifier]), Span) -> Maybe B.ByteString
doc ((a,_,_),_) = a
metas :: ((Maybe B.ByteString, [Metadata], [Modifier]), Span) -> [Metadata]
metas ((_,b,_),_) = b
mods :: ((Maybe B.ByteString, [Metadata], [Modifier]), Span) -> [Modifier]
mods ((_,_,c),_) = c

}
