{
module Kit.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as B
import Kit.Ast
import Kit.Error
import Kit.Parser.Base
import Kit.Parser.Span
import Kit.Parser.Token
import Kit.Str
}

%name parseTokens Statements
%name parseExpr Expr
%name parseTopLevelExpr TopLevelExpr
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
  -- '#' {(Hash,_)}
  '$' {(Dollar,_)}
  "=>" {(Arrow,_)}
  "->" {(FunctionArrow,_)}
  -- '?' {(Question,_)}
  '_' {(Underscore,_)}
  ".*" {(WildcardSuffix,_)}
  abstract {(KeywordAbstract,_)}
  as {(KeywordAs,_)}
  atom {(KeywordAtom,_)}
  break {(KeywordBreak,_)}
  const {(KeywordConst,_)}
  continue {(KeywordContinue,_)}
  default {(KeywordDefault,_)}
  defer {(KeywordDefer,_)}
  do {(KeywordDo,_)}
  else {(KeywordElse,_)}
  empty {(KeywordEmpty,_)}
  enum {(KeywordEnum,_)}
  for {(KeywordFor,_)}
  function {(KeywordFunction,_)}
  if {(KeywordIf,_)}
  implement {(KeywordImplement,_)}
  implicit {(KeywordImplicit,_)}
  import {(KeywordImport,_)}
  include {(KeywordInclude,_)}
  inline {(KeywordInline,_)}
  in {(KeywordIn,_)}
  match {(KeywordMatch,_)}
  null {(KeywordNull,_)}
  private {(KeywordPrivate,_)}
  public {(KeywordPublic,_)}
  return {(KeywordReturn,_)}
  rule {(KeywordRule,_)}
  rules {(KeywordRules,_)}
  Self {(KeywordSelf,_)}
  sizeof {(KeywordSizeof,_)}
  specialize {(KeywordSpecialize,_)}
  static {(KeywordStatic,_)}
  struct {(KeywordStruct,_)}
  then {(KeywordThen,_)}
  this {(KeywordThis,_)}
  throw {(KeywordThrow,_)}
  trait {(KeywordTrait,_)}
  type {(KeywordType,_)}
  typedef {(KeywordTypedef,_)}
  union {(KeywordUnion,_)}
  unsafe {(KeywordUnsafe,_)}
  using {(KeywordUsing,_)}
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
  float {(LiteralFloat _ _, _)}
  int {(LiteralInt _ _, _)}
  char {(LiteralChar _, _)}
%%

Statements :: {[Statement]}
  : Statements_ {reverse $1}
Statements_ :: {[Statement]}
  : {[]}
  | Statements_ Statement {$2 : $1}

MaybeDoc :: {Maybe Str}
  : {Nothing}
  | doc_comment {Just $ extract_doc_comment $1}

Statement :: {Statement}
  : import ModulePath ';' {ps (p $1 <+> p $3) $ Import (reverse $ fst $2) False}
  | import ModulePath ".*" ';' {ps (p $1 <+> p $3) $ Import (reverse $ fst $2) True}
  | include str ';' {ps (p $1 <+> p $3) $ Include $ B.unpack $ extract_lit $ fst $2}
  | using UsingClause ';' {ps (snd $1 <+> snd $3) $ ModuleUsing $ fst $2}
  | DocMetaMods typedef upper_identifier '=' TypeSpec ';' {
    ps (fp [p $1, p $2, p $6]) $ Typedef (extract_upper_identifier $3) (fst $5)
  }
  | DocMetaMods atom upper_identifier ';' {
    ps (fp [p $1, p $2, p $4]) $ TypeDeclaration $ (newTypeDefinition) {
      typeName = ns $ extract_upper_identifier $3,
      typeDoc = doc $1,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeRules = [],
      typeMethods = [],
      typeStaticMethods = [],
      typeParams = [],
      typeStaticFields = [],
      typePos = snd $2 <+> snd $3,
      typeSubtype = Atom
    }
  }
  | DocMetaMods enum upper_identifier TypeParams TypeAnnotation '{' RulesMethodsVariants '}' {
    ps (fp [p $1, p $2, p $8]) $ TypeDeclaration $ (newTypeDefinition) {
      typeName = ns $ extract_upper_identifier $3,
      typeDoc = doc $1,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeRules = reverse $ extractRules $7,
      typeMethods = reverse $ extractMethods $7,
      typeStaticFields = reverse $ extractStaticFields $7,
      typeStaticMethods = reverse $ extractStaticMethods $7,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = Enum {
        enumVariants = [v { variantParent = ns $ extract_upper_identifier $3} | v <- reverse $ extractVariants $7],
        enumUnderlyingType = case fst $5 of {Just x -> Just x; Nothing -> Just (TypeSpec ([], "Int") [] NoPos)}
      }
    }
  }
  | DocMetaMods struct upper_identifier TypeParams RulesMethodsFieldsBody {
    ps (fp [p $1, p $2, p $5]) $ TypeDeclaration $ (newTypeDefinition) {
      typeName = ns $ extract_upper_identifier $3,
      typeDoc = doc $1,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeRules = reverse $ extractRules $ fst $5,
      typeMethods = reverse $ extractMethods $ fst $5,
      typeStaticFields = reverse $ extractStaticFields $ fst $5,
      typeStaticMethods = reverse $ extractStaticMethods $ fst $5,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = Struct {
        structFields = reverse $ extractFields $ fst $5
      }
    }
  }
  | DocMetaMods union upper_identifier TypeParams RulesMethodsFieldsBody {
    ps (fp [p $1, p $2, p $5]) $ TypeDeclaration $ (newTypeDefinition) {
      typeName = ns $ extract_upper_identifier $3,
      typeDoc = doc $1,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeRules = reverse $ extractRules $ fst $5,
      typeMethods = reverse $ extractMethods $ fst $5,
      typeStaticFields = reverse $ extractStaticFields $ fst $5,
      typeStaticMethods = reverse $ extractStaticMethods $ fst $5,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = Union {
        unionFields = reverse $ extractFields $ fst $5
      }
    }
  }
  | DocMetaMods abstract upper_identifier TypeParams TypeAnnotation RulesMethodsBody {
    ps (fp [p $1, p $2, p $6]) $ TypeDeclaration $ (newTypeDefinition) {
      typeName = ns $ extract_upper_identifier $3,
      typeDoc = doc $1,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeRules = reverse $ extractRules $ fst $6,
      typeMethods = reverse $ extractMethods $ fst $6,
      typeStaticFields = reverse $ extractStaticFields $ fst $6,
      typeStaticMethods = reverse $ extractStaticMethods $ fst $6,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = Abstract {
        abstractUnderlyingType = fst $5
      }
    }
  }
  | DocMetaMods trait upper_identifier TypeParams AssocTypeDeclarations RulesMethodsBody {
    ps (fp [p $1, p $2, p $6]) $ TraitDeclaration $ newTraitDefinition {
      traitName = ns $ extract_upper_identifier $3,
      traitDoc = doc $1,
      traitMeta = reverse $ metas $1,
      traitModifiers = reverse $ mods $1,
      traitParams = fst $4,
      traitAssocParams = reverse $5,
      traitPos = snd $2 <+> snd $3,
      traitRules = reverse $ extractRules $ fst $6,
      traitMethods = reverse $ extractMethods $ fst $6
    }
  }
  | DocMetaMods implement TypeParams TypeSpec AssocTypes for TypeSpec MethodsBody {
    ps (fp [p $1, p $2, p $7]) $ Implement $ newTraitImplementation {
      implTrait = Just $ fst $4,
      implFor = Just $ fst $7,
      implParams = fst $3,
      implAssocTypes = map Just $ reverse $5,
      implMethods = reverse $ extractMethods $ fst $8,
      implDoc = doc $1,
      implPos = snd $2 <+> snd $4
    }
  }
  | DocMetaMods specialize TypeSpec as TypeSpec ';' {
    ps (fp [p $1, p $2, p $6]) $ Specialize (fst $3) (fst $5)
  }
  | MaybeDoc rules upper_identifier '{' ShortRules '}' {
    ps (snd $2 <+> snd $3) $ RuleSetDeclaration $ newRuleSet {
      ruleSetName = ns $ extract_upper_identifier $3,
      ruleSetPos = snd $2 <+> snd $3,
      ruleSetDoc = $1,
      ruleSetRules = $5
    }
  }
  | VarDefinition {ps (varPos $1) $ ModuleVarDeclaration $ $1}
  | FunctionDecl {$1}

AssocTypes :: {[TypeSpec]}
  : {[]}
  | '(' CommaDelimitedTypes ')' {reverse $2}

AssocTypeDeclarations :: {[TypeParam]}
  : {[]}
  | '(' TypeParams_ ')' {$2}

TopLevelExpr :: {Expr}
  : StandaloneExpr {$1}
  | var Identifier TypeAnnotation OptionalStandaloneDefault {pe (p $1 <+> p $4) $ VarDeclaration (fst $2) (fst $3) (fst $4)}
  | return TopLevelExpr {pe (p $1 <+> pos $2) $ Return $ Just $2}
  | return ';' {pe (p $1 <+> p $2) $ Return $ Nothing}
  | defer TopLevelExpr {pe (p $1 <+> pos $2) $ Defer $ $2}
  | throw TopLevelExpr {pe (p $1 <+> pos $2) $ Throw $2}
  | continue ';' {pe (p $1 <+> p $2) $ Continue}
  | break ';' {pe (p $1 <+> p $2) $ Break}
  -- | tokens LexMacroTokenBlock {pe (snd $1 <+> snd $2) $ TokenExpr $ tc (fst $2)}

StandaloneExpr :: {Expr}
  : ExprBlock {$1}
  | using UsingClauses StandaloneExpr {pe (snd $1 <+> snd $2) $ Using (reverse $ fst $2) $3}
  | if BinopTermOr ExprBlock else StandaloneExpr {pe (p $1 <+> pos $5) $ If $2 $3 (Just $5)}
  | if BinopTermOr ExprBlock {pe (p $1 <+> pos $3) $ If $2 $3 (Nothing)}
  | for Identifier in Expr ExprBlock {pe (p $1 <+> pos $5) $ For (pe (snd $2) (Identifier (fst $2))) $4 $5}
  | while Expr ExprBlock {pe (p $1 <+> pos $3) $ While $2 $3 False}
  | do ExprBlock while Expr ';' {pe (p $1 <+> p $5) $ While $4 $2 True}
  | match Expr '{' MatchCases DefaultMatchCase '}' {pe (p $1 <+> p $6) $ Match $2 (reverse $4) $5}
  | Expr ';' {me (pos $1 <+> p $2) $1}

UsingClauses :: {([UsingType Expr (Maybe TypeSpec)], Span)}
  : UsingClause {([fst $1], snd $1)}
  | UsingClauses ',' UsingClause {(fst $3 : fst $1, snd $1 <+> snd $3)}

UsingClause :: {(UsingType Expr (Maybe TypeSpec), Span)}
  : rules TypePath {(UsingRuleSet $ Just $ TypeSpec (fst $2) [] (snd $2), snd $1 <+> snd $2)}
  | implicit Expr {(UsingImplicit $ $2, snd $1 <+> pos $2)}

FunctionDecl :: {Statement}
  : DocMetaMods function identifier TypeParams '(' VarArgs ')' TypeAnnotation OptionalBody {
    ps (fp [p $2 <+> p $3]) $ FunctionDeclaration $ (newFunctionDefinition :: FunctionDefinition Expr (Maybe TypeSpec)) {
      functionName = ns $ extract_identifier $3,
      functionDoc = doc $1,
      functionMeta = reverse $ metas $1,
      functionModifiers = reverse $ mods $1,
      functionParams = fst $4,
      functionType = fst $8,
      functionArgs = reverse (fst $6),
      functionBody = fst $9,
      functionVarargs = snd $6,
      functionPos = snd $2 <+> snd $3
    }
  }

StaticFunctionDecl :: {FunctionDefinition Expr (Maybe TypeSpec)}
  : StaticDocMetaMods function identifier TypeParams '(' VarArgs ')' TypeAnnotation OptionalBody {
    (newFunctionDefinition :: FunctionDefinition Expr (Maybe TypeSpec)) {
      functionName = ns $ extract_identifier $3,
      functionDoc = doc $1,
      functionMeta = reverse $ metas $1,
      functionModifiers = reverse $ mods $1,
      functionParams = fst $4,
      functionType = fst $8,
      functionArgs = reverse (fst $6),
      functionBody = fst $9,
      functionVarargs = snd $6,
      functionPos = snd $2 <+> snd $3
    }
  }

MatchCases :: {[MatchCase Expr]}
  : {[]}
  | MatchCases MatchCase {$2 : $1}

MatchCase :: {MatchCase Expr}
  : Expr "=>" TopLevelExpr {MatchCase {matchPattern = $1, matchBody = $3}}

DefaultMatchCase :: {Maybe Expr}
  : {Nothing}
  | default "=>" TopLevelExpr {Just $3}

ExprBlock :: {Expr}
  : '{' MacroIdentifier '}' {pe (snd $1 <+> snd $3) $ Identifier (fst $2)}
  | '{' TopLevelExprs '}' {pe (p $1 <+> p $3) $ Block $ reverse $2}

TopLevelExprs :: {[Expr]}
  : {[]}
  | TopLevelExprs TopLevelExpr {$2 : $1}

ModulePath :: {([Str], Span)}
  : identifier {([extract_identifier $1], snd $1)}
  | ModulePath '.' identifier {(extract_identifier $3 : fst $1, snd $1 <+> p $3)}

CallArgs :: {[Expr]}
  : {[]}
  | Expr {[$1]}
  | CallArgs ',' Expr {$3 : $1}

MetaArg :: {MetaArg}
  : Term {MetaLiteral $ fst $1}
  | UpperOrLowerIdentifier {MetaIdentifier $ fst $1}

MetaArgs :: {[MetaArg]}
  : MetaArg {[$1]}
  | MetaArgs ',' MetaArg {$3 : $1}

Metadata :: {(Metadata, Span)}
  : "#[" identifier '(' MetaArgs ')' ']' {(Metadata {metaName = extract_identifier $2, metaArgs = reverse $4}, (p $1) <+> (p $6))}
  | "#[" identifier ']' {(Metadata {metaName = extract_identifier $2, metaArgs = []}, (p $1) <+> (p $3))}

MetaMods :: {(([Metadata], [Modifier]), Span)}
  : {(([], []), NoPos)}
  | MetaMods Metadata {let (meta, mods) = fst $1 in (((fst $2 : meta, mods)), snd $1 <+> snd $2)}
  | MetaMods public {let (meta, mods) = fst $1 in (((meta, Public : mods)), snd $1 <+> snd $2)}
  | MetaMods private {let (meta, mods) = fst $1 in (((meta, Private : mods)), snd $1 <+> snd $2)}
  | MetaMods inline {let (meta, mods) = fst $1 in (((meta, Inline : mods)), snd $1 <+> snd $2)}
  | MetaMods const {let (meta, mods) = fst $1 in ((meta, (Const : mods)), snd $1 <+> snd $2)}

DocMetaMods :: {((Maybe Str, [Metadata], [Modifier]), Span)}
  : MetaMods {((Nothing, fst $ fst $1, snd $ fst $1), p $1)}
  | doc_comment MetaMods {((Just $ extract_doc_comment $1, fst $ fst $2, snd $ fst $2), p $2)}

StaticDocMetaMods :: {((Maybe Str, [Metadata], [Modifier]), Span)}
  : DocMetaMods static MetaMods {
    let ((doc, meta1, mod1), span1) = $1 in
    let ((meta2, mod2), span2) = $3 in
    ((doc, meta1 ++ meta2, mod1 ++ (Static : mod2)), span1 <+> span2)
  }

TypeAnnotation :: {(Maybe TypeSpec, Span)}
  : {(Nothing, NoPos)}
  | ':' TypeSpec {(Just $ fst $2, p $1 <+> p $2)}

TypeSpec :: {(TypeSpec, Span)}
  : TypePath TypeSpecParams {(TypeSpec (fst $1) (fst $2) (p $1 <+> p $2), p $1 <+> p $2)}
  | function FunctionTypeSpec {$2}
  | TupleTypeSpec {$1}
  | Term {(ConstantTypeSpec (fst $1) (snd $1), snd $1)}

TupleTypeSpec :: {(TypeSpec, Span)}
  : '(' TypeSpec ',' CommaDelimitedTypes ')' {let p = snd $1 <+> snd $5 in (TupleTypeSpec ((fst $2) : (reverse $4)) p, p)}

FunctionTypeSpec :: {(TypeSpec, Span)}
  : '(' ')' "->" TypeSpec {let p = snd $1 <+> snd $4 in (FunctionTypeSpec (fst $4) [] False p, p)}
  | '(' CommaDelimitedTypes ',' "..." ')' "->" TypeSpec {let p = snd $1 <+> snd $6 in (FunctionTypeSpec (fst $7) (reverse $2) True p, p)}
  | '(' CommaDelimitedTypes ')' "->" TypeSpec {let p = snd $1 <+> snd $5 in (FunctionTypeSpec (fst $5) (reverse $2) False p, p)}

CommaDelimitedTypes :: {[TypeSpec]}
  : TypeSpec {[fst $1]}
  | CommaDelimitedTypes ',' TypeSpec {fst $3 : $1}

OptionalBody :: {(Maybe Expr, Span)}
  : ';' {(Nothing, NoPos)}
  | using UsingClauses ExprBlock {(Just $ pe (snd $1 <+> snd $2) $ Using (fst $2) $3, pos $3)}
  | ExprBlock {(Just $1, pos $1)}

OptionalRuleBody :: {Maybe Expr}
  : ';' {Nothing}
  | "=>" StandaloneExpr {Just $2}

TypeParams :: {([TypeParam], Span)}
  : {([], NoPos)}
  | '[' TypeParams_ ']' {(reverse $2, p $1 <+> p $3)}

TypeParams_ :: {[TypeParam]}
  : TypeParam {[$1]}
  | TypeParams_ ',' TypeParam {$3 : $1}

TypeParam :: {TypeParam}
  : upper_identifier TypeConstraints {TypeParam {paramName = (extract_upper_identifier $1), constraints = $2}}

TypeSpecParams :: {([TypeSpec], Span)}
  : {([], NoPos)}
  | '[' TypeSpecParams_ ']' {(reverse $2, p $1 <+> p $3)}

TypeSpecParams_ :: {[TypeSpec]}
  : TypeSpec {[fst $1]}
  | TypeSpecParams_ ',' TypeSpec {(fst $3) : $1}

TypeConstraints :: {[TypeSpec]}
  : {[]}
  | ':' TypeSpec {[fst $2]}
  | "::" TypeConstraints_ {reverse $2}

TypeConstraints_ :: {[TypeSpec]}
  : TypeSpec {[fst $1]}
  | TypeConstraints_ '|' TypeSpec {fst $3 : $1}

UpperOrLowerIdentifier :: {(Str, Span)}
  : identifier {(extract_identifier $1, snd $1)}
  | upper_identifier {(extract_upper_identifier $1, snd $1)}

TypePath :: {(TypePath, Span)}
  : ModulePath '.' Self {((fst $1, B.pack "Self"), p $1 <+> p $3)}
  | ModulePath '.' upper_identifier {((fst $1, extract_upper_identifier $3), p $1 <+> p $3)}
  | UpperOrLowerIdentifier {(([], fst $1), p $1)}
  | Self {(([], B.pack "Self"), p $1)}

VarDefinition :: {VarDefinition Expr (Maybe TypeSpec)}
  : DocMetaMods var UpperOrLowerIdentifier TypeAnnotation OptionalDefault ';' {
    newVarDefinition {
      varName = ns $ fst $ $3,
      varDoc = doc $1,
      varMeta = reverse (metas $1),
      varType = fst $4,
      varModifiers = reverse (mods $1),
      varDefault = $5,
      varPos = p $2 <+> p $3
    }
  }

StaticVarDefinition :: {VarDefinition Expr (Maybe TypeSpec)}
  : StaticDocMetaMods var UpperOrLowerIdentifier TypeAnnotation OptionalDefault ';' {
    newVarDefinition {
      varName = ns $ fst $ $3,
      varDoc = doc $1,
      varMeta = reverse (metas $1),
      varType = fst $4,
      varModifiers = reverse (mods $1),
      varDefault = $5,
      varPos = p $2 <+> p $3
    }
  }

OptionalStandaloneDefault :: {(Maybe Expr, Span)}
  : ';' {(Nothing, snd $1)}
  | '=' StandaloneExpr {(Just $2, snd $1 <+> pos $2)}

OptionalDefault :: {Maybe Expr}
  : {Nothing}
  | '=' Expr {Just $2}

EnumPrefix :: {((DocMetaMod, Str), Span)}
  : DocMetaMods upper_identifier {(($1, extract_upper_identifier $2), snd $2)}

EnumVariant :: {EnumVariant Expr (Maybe TypeSpec)}
  : EnumPrefix ';' {
      newEnumVariant {
        variantName = ([], snd $ fst $1),
        variantDoc = doc $ fst $ fst $1,
        variantMeta = reverse $ metas $ fst $ fst $1,
        variantModifiers = reverse $ mods $ fst $ fst $1,
        variantArgs = [],
        variantValue = Nothing,
        variantPos = snd $1
      }
    }
  | EnumPrefix '=' Expr ';' {
      newEnumVariant {
        variantName = ([], snd $ fst $1),
        variantDoc = doc $ fst $ fst $1,
        variantMeta = reverse $ metas $ fst $ fst $1,
        variantModifiers = reverse $ mods $ fst $ fst $1,
        variantArgs = [],
        variantValue = Just $3,
        variantPos = snd $1
      }
    }
  | EnumPrefix '(' Args ')' ';' {
      newEnumVariant {
        variantName = ([], snd $ fst $1),
        variantDoc = doc $ fst $ fst $1,
        variantMeta = reverse $ metas $ fst $ fst $1,
        variantModifiers = reverse $ mods $ fst $ fst $1,
        variantArgs = reverse $3,
        variantValue = Nothing,
        variantPos = snd $1
      }
    }

VarArgs :: {([ArgSpec Expr (Maybe TypeSpec)], Bool)}
  : Args ',' "..." {($1, True)}
  | Args {($1, False)}

Args :: {[ArgSpec Expr (Maybe TypeSpec)]}
  : {[]}
  | ArgSpec {[$1]}
  | Args ',' ArgSpec {$3 : $1}

ArgSpec  :: {ArgSpec Expr (Maybe TypeSpec)}
  : identifier TypeAnnotation OptionalDefault {ArgSpec {
    argName = extract_identifier $1,
     argType = fst $2,
     argDefault = $3,
     argPos = p $1 <+> p $2 <+> (case $3 of { Just x -> pos x; Nothing -> NoPos })
    }
  }

RulesMethods :: {[Member Expr (Maybe TypeSpec)]}
  : {[]}
  | RulesMethods RewriteRule {(RuleMember $2) : $1}
  | RulesMethods RuleBlock {(map RuleMember $2) ++ $1}
  | RulesMethods Method {$2 : $1}
  | RulesMethods StaticVarDefinition {StaticFieldMember $2 : $1}

RulesMethodsFields :: {[Member Expr (Maybe TypeSpec)]}
  : {[]}
  | RulesMethodsFields RewriteRule {(RuleMember $2) : $1}
  | RulesMethodsFields RuleBlock {(map RuleMember $2) ++ $1}
  | RulesMethodsFields Method {$2 : $1}
  | RulesMethodsFields StaticVarDefinition {StaticFieldMember $2 : $1}
  | RulesMethodsFields VarDefinition {(FieldMember $2 : $1)}

RulesMethodsVariants :: {[Member Expr (Maybe TypeSpec)]}
  : {[]}
  | RulesMethodsVariants RewriteRule {(RuleMember $2) : $1}
  | RulesMethodsVariants RuleBlock {(map RuleMember $2) ++ $1}
  | RulesMethodsVariants Method {$2 : $1}
  | RulesMethodsVariants StaticVarDefinition {StaticFieldMember $2 : $1}
  | RulesMethodsVariants EnumVariant {(VariantMember $2 : $1)}

Methods :: {[Member Expr (Maybe TypeSpec)]}
  : {[]}
  | Methods Method {$2 : $1}

Method :: {Member Expr (Maybe TypeSpec)}
  : FunctionDecl {
    case stmt $1 of
      FunctionDeclaration f -> MethodMember f
  }
  | StaticFunctionDecl {
    StaticMethodMember $1
  }

RulesMethodsBody :: {([Member Expr (Maybe TypeSpec)], Span)}
  : '{' RulesMethods '}' {($2, p $1 <+> p $3)}
  | ';' {([], p $1)}

RulesMethodsFieldsBody :: {([Member Expr (Maybe TypeSpec)], Span)}
  : '{' RulesMethodsFields '}' {($2, p $1 <+> p $3)}
  | ';' {([], p $1)}

MethodsBody :: {([Member Expr (Maybe TypeSpec)], Span)}
  : '{' Methods '}' {($2, p $1 <+> p $3)}
  | ';' {([], p $1)}

RewriteExpr :: {Expr}
  : Expr {$1}
  | StandaloneExpr {$1}

RewriteRule :: {RewriteRule Expr (Maybe TypeSpec)}
  : MaybeDoc rule '(' RewriteExpr ')' OptionalRuleBody {
    newRewriteRule {
      ruleDoc = $1,
      rulePattern = $4,
      ruleBody = $6,
      rulePos = snd $2 <+> snd $5
    }
  }

RuleBlock :: {[RewriteRule Expr (Maybe TypeSpec)]}
  : rules '{' ShortRules '}' {$3}

ShortRules :: {[RewriteRule Expr (Maybe TypeSpec)]}
  : {[]}
  | ShortRules ShortRule {$2 : $1}

ShortRule :: {RewriteRule Expr (Maybe TypeSpec)}
  : MaybeDoc '(' RewriteExpr ')' OptionalRuleBody {
    newRewriteRule {
      ruleDoc = $1,
      rulePattern = $3,
      ruleBody = $5,
      rulePos = snd $2 <+> snd $4
    }
  }

Expr :: {Expr}
  : RangeLiteral {$1}

RangeLiteral :: {Expr}
  : BinopTermAssign "..." BinopTermAssign {pe (pos $1 <+> pos $3) $ RangeLiteral $1 $3}
  | BinopTermAssign {$1}

BinopTermAssign :: {Expr}
  : BinopTermCons {$1}
  | BinopTermCons '=' BinopTermAssign {pe (pos $1 <+> pos $3) $ Binop Assign $1 $3}
  | BinopTermCons assign_op BinopTermAssign {pe (pos $1 <+> pos $3) $ Binop (AssignOp (extract_assign_op $2)) $1 $3}
BinopTermCons :: {Expr}
  : BinopTermTernary {$1}
  | BinopTermTernary "::" BinopTermCons {pe (pos $1 <+> pos $3) $ Binop Cons $1 $3}
-- TokenExpr :: {Expr}
--   : token LexMacroTokenAny {pe (snd $1 <+> snd $2) $ TokenExpr $ tc [$2]}
  -- | BinopTermTernary {$1}
BinopTermTernary :: {Expr}
  : if BinopTermOr then BinopTermOr else BinopTermTernary {pe (p $1 <+> pos $6) $ If $2 $4 (Just $6)}
  | BinopTermOr {$1}
BinopTermOr :: {Expr}
  : BinopTermAnd {$1}
  | BinopTermOr "||" BinopTermAnd {pe (pos $1 <+> pos $3) $ Binop Or $1 $3}
BinopTermAnd :: {Expr}
  : BinopTermEq {$1}
  | BinopTermAnd "&&" BinopTermEq {pe (pos $1 <+> pos $3) $ Binop And $1 $3}
BinopTermEq :: {Expr}
  : BinopTermCompare {$1}
  | BinopTermEq "==" BinopTermCompare {pe (pos $1 <+> pos $3) $ Binop Eq $1 $3}
  | BinopTermEq "!=" BinopTermCompare {pe (pos $1 <+> pos $3) $ Binop Neq $1 $3}
BinopTermCompare :: {Expr}
  : BinopTermBitOr {$1}
  | BinopTermCompare '>' BinopTermBitOr {pe (pos $1 <+> pos $3) $ Binop Gt $1 $3}
  | BinopTermCompare '<' BinopTermBitOr {pe (pos $1 <+> pos $3) $ Binop Lt $1 $3}
  | BinopTermCompare ">=" BinopTermBitOr {pe (pos $1 <+> pos $3) $ Binop Gte $1 $3}
  | BinopTermCompare "<=" BinopTermBitOr {pe (pos $1 <+> pos $3) $ Binop Lte $1 $3}
BinopTermBitOr :: {Expr}
  : BinopTermBitXor {$1}
  | BinopTermBitOr '|' BinopTermBitXor {pe (pos $1 <+> pos $3) $ Binop BitOr $1 $3}
BinopTermBitXor :: {Expr}
  : BinopTermBitAnd {$1}
  | BinopTermBitXor '^' BinopTermBitAnd {pe (pos $1 <+> pos $3) $ Binop BitXor $1 $3}
BinopTermBitAnd :: {Expr}
  : BinopTermShift {$1}
  | BinopTermBitAnd '&' BinopTermShift {pe (pos $1 <+> pos $3) $ Binop BitAnd $1 $3}
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
  | BinopTermMul '%' BinopTermCustom {pe (pos $1 <+> pos $3) $ Binop Mod $1 $3}
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
  : '[' ArrayElems ']' {pe (p $1 <+> p $3) $ ArrayLiteral $ reverse $2}
  | '[' ArrayElems ',' ']' {pe (p $1 <+> p $4) $ ArrayLiteral $ reverse $2}
  | '[' ']' { pe (p $1 <+> p $2) $ ArrayLiteral []}
  | CastExpr {$1}

ArrayElems :: {[Expr]}
  : Expr {[$1]}
  | ArrayElems ',' Expr {$3 : $1}

CastExpr :: {Expr}
  : CastExpr as TypeSpec {pe (pos $1 <+> snd $3) $ Cast $1 (Just $ fst $3)}
  | ArrayAccessCallFieldExpr {$1}

ArrayAccessCallFieldExpr :: {Expr}
  : ArrayAccessCallFieldExpr '[' Expr ']' {pe (pos $1 <+> p $4) $ ArrayAccess $1 $3}
  | ArrayAccessCallFieldExpr '(' CallArgs ')' {pe (pos $1 <+> p $4) $ Call $1 (reverse $3)}
  | ArrayAccessCallFieldExpr '.' Identifier {pe (pos $1 <+> p $3) $ Field $1 $ fst $3}
  | TypeAnnotatedExpr {$1}

TypeAnnotatedExpr :: {Expr}
  : TypeAnnotatedExpr ':' TypeSpec {pe (pos $1 <+> snd $3) $ TypeAnnotation $1 (Just $ fst $3)}
  | BaseExpr {$1}

BaseExpr :: {Expr}
  : Term {pe (snd $1) (Literal $ fst $1)}
  | this {pe (snd $1) This}
  | Self {pe (snd $1) Self}
  | Identifier {pe (snd $1) $ Identifier (fst $1)}
  | unsafe Expr {pe (p $1 <+> pos $2) (Unsafe $2)}
  | sizeof TypeSpec {pe (snd $1 <+> snd $2) (SizeOf $ Just $ fst $2)}
  | '(' Expr ParenthesizedExprs ')' {if null $3 then $2 else pe (snd $1 <+> snd $4) (TupleInit ($2 : reverse $3)) }
  | null {pe (snd $1) Null}
  | empty {pe (snd $1) Empty}
  | struct TypeSpec '{' StructInitFields '}' {pe (p $1 <+> p $5) $ StructInit (Just $ fst $2) $4}
  | implicit TypeSpec {pe (snd $1 <+> snd $2) $ Implicit $ Just $ fst $2}

ParenthesizedExprs :: {[Expr]}
  : {[]}
  | ParenthesizedExprs ',' Expr {$3 : $1}

Term :: {(ValueLiteral (Maybe TypeSpec), Span)}
  : bool {(BoolValue $ extract_bool $ fst $1, snd $1)}
  | str {(StringValue $ extract_lit $ fst $1, snd $1)}
  | int {let x = extract_int_lit $ fst $1 in (IntValue (fst x) (snd x), snd $1)}
  | float {let x = extract_float_lit $ fst $1 in (FloatValue (fst x) (snd x), snd $1)}
  | char {(CharValue $ extract_char_lit $ fst $1, snd $1)}

StructInitFields :: {[(Str, Expr)]}
  : {[]}
  | OneOrMoreStructInitFields ',' {reverse $1}
  | OneOrMoreStructInitFields {reverse $1}

OneOrMoreStructInitFields :: {[(Str, Expr)]}
  : StructInitField {[$1]}
  | OneOrMoreStructInitFields ',' StructInitField {$3 : $1}

StructInitField :: {(Str, Expr)}
  : UpperOrLowerIdentifier ':' Expr {(fst $1, $3)}
  | UpperOrLowerIdentifier {(fst $1, pe (snd $1) $ Identifier (Var $ ([], fst $1)))}

Identifier :: {(Identifier (Maybe TypeSpec), Span)}
  : UpperOrLowerIdentifier {(Var $ ([], fst $1), snd $1)}
  | MacroIdentifier {$1}
  | '_' {(Hole, snd $1)}

MacroIdentifier :: {(Identifier (Maybe TypeSpec), Span)}
  : macro_identifier {(MacroVar (extract_macro_identifier $1) Nothing, snd $1)}
  | '$' '{' UpperOrLowerIdentifier TypeAnnotation '}' {(MacroVar (fst $3) (fst $4), (p $1 <+> p $5))}
  | '$' '{' '_' TypeAnnotation '}' {(MacroVar "_" (fst $4), (p $1 <+> p $5))}

-- LexMacroTokenBlock :: {([Token], Span)}
--   : '{' LexMacroTokensInsideBlock '}' {(reverse $2, snd $1 <+> snd $3)}

-- LexMacroTokensInsideBlock :: {[Token]}
--   : {[]}
--   | LexMacroTokensInsideBlock LexMacroToken {$2 : $1}
--   | LexMacroTokensInsideBlock LexMacroTokenBlock {((CurlyBraceClose, NoPos) : (reverse $ fst $2)) ++ ((CurlyBraceOpen, NoPos) : $1)}

-- LexMacroTokenAny :: {Token}
--   : '{' {$1}
--   | '}' {$1}
--   | LexMacroToken {$1}

-- LexMacroToken :: {Token}
--   : '[' {$1}
--   | ']' {$1}
--   | '(' {$1}
--   | ')' {$1}
--   | ':' {$1}
--   | "#[" {$1}
--   | "..." {$1}
--   | '.' {$1}
--   | '#' {$1}
--   | '$' {$1}
--   | "=>" {$1}
--   | '?' {$1}
--   | ',' {$1}
--   | ';' {$1}
--   | abstract {$1}
--   | as {$1}
--   | atom {$1}
--   | break {$1}
--   | case {$1}
--   | code {$1}
--   | const {$1}
--   | continue {$1}
--   | default {$1}
--   | do {$1}
--   | else {$1}
--   | enum {$1}
--   | for {$1}
--   | function {$1}
--   | if {$1}
--   | implement {$1}
--   | import {$1}
--   | include {$1}
--   | inline {$1}
--   | in {$1}
--   | macro {$1}
--   | match {$1}
--   | op {$1}
--   | override {$1}
--   | private {$1}
--   | public {$1}
--   | return {$1}
--   | rule {$1}
--   | rules {$1}
--   | Self {$1}
--   | sizeof {$1}
--   | static {$1}
--   | struct {$1}
--   | super {$1}
--   | switch {$1}
--   | then {$1}
--   | this {$1}
--   | throw {$1}
--   | token {$1}
--   | tokens {$1}
--   | trait {$1}
--   | typedef {$1}
--   | unsafe {$1}
--   | var {$1}
--   | while {$1}
--   | doc_comment {$1}
--   | identifier {$1}
--   | macro_identifier {$1}
--   | upper_identifier {$1}
--   | "++" {$1}
--   | "--" {$1}
--   | '+' {$1}
--   | '-' {$1}
--   | '*' {$1}
--   | '/' {$1}
--   | '%' {$1}
--   | "==" {$1}
--   | "!=" {$1}
--   | ">=" {$1}
--   | "<=" {$1}
--   | "<<" {$1}
--   | ">>" {$1}
--   | '>' {$1}
--   | '<' {$1}
--   | "&&" {$1}
--   | "||" {$1}
--   | '&' {$1}
--   | '|' {$1}
--   | '^' {$1}
--   | '=' {$1}
--   | assign_op {$1}
--   | '!' {$1}
--   | '~' {$1}
--   | "::" {$1}
--   | custom_op {$1}
--   | bool {$1}
--   | str {$1}
--   | float {$1}
--   | int {$1}
{

thenP = (>>=)
returnP = return

parseError [] = Err $ KitError $ ParseError ("Unexpected end of input") (Nothing)
parseError t = Err $ KitError $ ParseError ("Unexpected " ++ (show $ fst et)) (Just $ snd et) where et = head t

data Member a b
  = RuleMember (RewriteRule a b)
  | FieldMember (VarDefinition a b)
  | MethodMember (FunctionDefinition a b)
  | StaticFieldMember (VarDefinition a b)
  | StaticMethodMember (FunctionDefinition a b)
  | VariantMember (EnumVariant a b)

extractRules = foldr (\m acc -> case m of {RuleMember x -> x : acc; _ -> acc}) []
extractFields = foldr (\m acc -> case m of {FieldMember x -> x : acc; _ -> acc}) []
extractMethods = foldr (\m acc -> case m of {MethodMember x -> x : acc; _ -> acc}) []
extractStaticFields = foldr (\m acc -> case m of {StaticFieldMember x -> x : acc; _ -> acc}) []
extractStaticMethods = foldr (\m acc -> case m of {StaticMethodMember x -> x : acc; _ -> acc}) []
extractVariants = foldr (\m acc -> case m of {VariantMember x -> x : acc; _ -> acc}) []

-- projections
extract_identifier (LowerIdentifier x,_) = x
extract_macro_identifier (MacroIdentifier x,_) = x
extract_upper_identifier (UpperIdentifier x,_) = x
extract_bool (LiteralBool x) = x
extract_lit (LiteralString x) = x
extract_int_lit (LiteralInt x y) = (x, numSpec y)
extract_float_lit (LiteralFloat x y) = (x, numSpec y)
extract_char_lit (LiteralChar x) = x
extract_assign_op (Op (AssignOp x),_) = x
extract_custom_op (Op (Custom x),_) = x
extract_doc_comment (DocComment x,_) = x
numSpec (Just x) = Just $ TypeSpec ([], s_pack $ show x) [] NoPos
numSpec Nothing = Nothing

ns x = ([], x)

tc :: [Token] -> [TokenClass]
tc t = [fst t' | t' <- t]

p = snd

fp :: [Span] -> Span
fp s = foldr (<+>) NoPos s

type DocMetaMod = ((Maybe Str, [Metadata], [Modifier]), Span)

doc :: ((Maybe Str, [Metadata], [Modifier]), Span) -> Maybe Str
doc ((a,_,_),_) = a
metas :: ((Maybe Str, [Metadata], [Modifier]), Span) -> [Metadata]
metas ((_,b,_),_) = b
mods :: ((Maybe Str, [Metadata], [Modifier]), Span) -> [Modifier]
mods ((_,_,c),_) = c

}
