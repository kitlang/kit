{
module Kit.Parser.Parser where

import qualified Data.ByteString.Char8 as B
import Kit.Ast
import Kit.Error
import Kit.Parser.Base
import Kit.Parser.Token
import Kit.Str
}

%name parseTokens Statements
%name parseExpr Expr
%name parseTopLevelExpr TopLevelExpr
%name parseTypeSpec TypeSpec
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
  ".**" {(DoubleWildcardSuffix,_)}
  abstract {(KeywordAbstract,_)}
  as {(KeywordAs,_)}
  break {(KeywordBreak,_)}
  const {(KeywordConst,_)}
  continue {(KeywordContinue,_)}
  default {(KeywordDefault,_)}
  defer {(KeywordDefer,_)}
  do {(KeywordDo,_)}
  else {(KeywordElse,_)}
  empty {(KeywordEmpty,_)}
  enum {(KeywordEnum,_)}
  extend {(KeywordExtend,_)}
  for {(KeywordFor,_)}
  function {(KeywordFunction,_)}
  if {(KeywordIf,_)}
  implement {(KeywordImplement,_)}
  implicit {(KeywordImplicit,_)}
  import {(KeywordImport,_)}
  include {(KeywordInclude,_)}
  inline {(KeywordInline,_)}
  in {(KeywordIn,_)}
  macro {(KeywordMacro,_)}
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
  tokens {(KeywordTokens,_)}
  trait {(KeywordTrait,_)}
  typedef {(KeywordTypedef,_)}
  undefined {(KeywordUndefined,_)}
  union {(KeywordUnion,_)}
  unsafe {(KeywordUnsafe,_)}
  using {(KeywordUsing,_)}
  var {(KeywordVar,_)}
  while {(KeywordWhile,_)}
  yield {(KeywordYield,_)}
  identifier {(LowerIdentifier _,_)}
  macro_identifier {(MacroIdentifier _,_)}
  upper_identifier {(UpperIdentifier _,_)}
  inline_c {(InlineC _,_)}
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

Statements :: {[SyntacticStatement]}
  : Statements_ {reverse $1}
Statements_ :: {[SyntacticStatement]}
  : {[]}
  | Statements_ Statement {$2 : $1}
  | Statements_ TypeDefinition DefinitionBody {
    let s = (ps (typePos $2) $ TypeDeclaration $2) in
    case $3 of
      Just x -> (x $ TypeSpec (typeName $2)  [] (typePos $2)): s : $1
      Nothing -> s : $1
  }
  | Statements_ TraitDefinition DefinitionBody {
    let s = (ps (traitPos $2) $ TraitDeclaration $2) in
    case $3 of
      Just x -> (x $ TypeSpec (traitName $2) [] (traitPos $2)) : s : $1
      Nothing -> s : $1
  }
  | Statements_ TraitImplementation DefinitionBody {
    case $3 of
      Just x -> let ExtendDefinition _ extensions = stmt (x $ implFor $2) in (ps (implPos $2) $ Implement $ foldr addImplExtension $2 extensions) : $1
      Nothing -> (ps (implPos $2) $ Implement $2) : $1
  }


Statement :: {SyntacticStatement}
  : import ModulePath ';' {ps (snd $1 <+> snd $3) $ Import (reverse $ fst $2) ImportSingle}
  | import ModulePath ".*" ';' {ps (snd $1 <+> snd $3) $ Import (reverse $ fst $2) ImportWildcard}
  | import ModulePath ".**" ';' {ps (snd $1 <+> snd $3) $ Import (reverse $ fst $2) ImportDoubleWildcard}
  | include str "=>" str ';' {ps (snd $1 <+> snd $5) $ Include (B.unpack $ extract_lit $ fst $2) (Just $ extract_lit $ fst $4)}
  | include str ';' {ps (snd $1 <+> snd $3) $ Include (B.unpack $ extract_lit $ fst $2) Nothing}
  | using UsingClause ';' {ps (snd $1 <+> snd $3) $ ModuleUsing $ fst $2}
  | MetaMods typedef upper_identifier '=' TypeSpec ';' {
    ps (fp [snd $1, snd $2, snd $6]) $ Typedef ([], extract_upper_identifier $3) (fst $5)
  }
  | MetaMods default TypeSpec as TypeSpec ';' {
    ps (fp [snd $1, snd $2, snd $6]) $ TraitDefault (fst $3) (fst $5)
  }
  | rules upper_identifier '{' ShortRules '}' {
    ps (snd $1 <+> snd $2) $ RuleSetDeclaration $ newRuleSet {
      ruleSetName = ns $ extract_upper_identifier $2,
      ruleSetPos = snd $1 <+> snd $2,
      ruleSetRules = $4
    }
  }
  | VarDefinition {ps (varPos $1) $ VarDeclaration $ $1}
  | FunctionDefinition {ps (functionPos $1) $ FunctionDeclaration $1}
  | extend TypePath '{' DefStatements '}' {
      ps (snd $1 <+> snd $5) $ ExtendDefinition (TypeSpec (fst $2) [] (snd $2)) $4
  }
  | macro FunctionDefinitionBase {ps (snd $1 <+> functionPos $2) $ MacroDeclaration $ $2 {
    functionPos = functionPos $2 <+> snd $1
  }}
  | TypePath '(' CallArgs ')' ';' {
    ps (snd $1 <+> snd $5) $ MacroCall (fst $1) $ reverse $3
  }

TypeDefinition :: {TypeDefinition Expr TypeSpec}
  : MetaMods enum upper_identifier TypeParams {
    newTypeDefinition {
      typeName = ns $ extract_upper_identifier $3,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = Enum {
        enumVariants = [],
        -- TODO
        enumUnderlyingType = (TypeSpec ([], "Int") [] NoPos)
      }
    }
  }
  | MetaMods struct upper_identifier TypeParams {
    newTypeDefinition {
      typeName = ns $ extract_upper_identifier $3,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = StructUnion {
        structUnionFields = [],
        isStruct = True
      }
    }
  }
  | MetaMods union upper_identifier TypeParams {
    newTypeDefinition {
      typeName = ns $ extract_upper_identifier $3,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = StructUnion {
        structUnionFields = [],
        isStruct = False
      }
    }
  }
  | MetaMods abstract upper_identifier TypeParams TypeAnnotation {
    newTypeDefinition {
      typeName = ns $ extract_upper_identifier $3,
      typeMeta = reverse $ metas $1,
      typeModifiers = reverse $ mods $1,
      typeParams = fst $4,
      typePos = snd $2 <+> snd $3,
      typeSubtype = Abstract {
        abstractUnderlyingType = fst $5
      }
    }
  }

TraitDefinition :: {TraitDefinition Expr TypeSpec}
  : MetaMods trait upper_identifier TypeParams AssocTypeDeclarations {
    newTraitDefinition {
      traitName = ns $ extract_upper_identifier $3,
      traitMeta = reverse $ metas $1,
      traitModifiers = reverse $ mods $1,
      traitParams = fst $4,
      traitAssocParams = reverse $5,
      traitPos = snd $2 <+> snd $3
    }
  }

TraitImplementation :: {TraitImplementation Expr TypeSpec}
  : implement TypeParams TypeSpec AssocTypes for TypeSpec {
    newTraitImplementation {
      implTrait = fst $3,
      implFor = fst $6,
      implParams = fst $2,
      implAssocTypes = reverse $4,
      implPos = snd $1 <+> snd $3
    }
  }

-- dummy rule to avoid larger rewrites for "commented out" functionality
NotYetSupported :: {Bool}
  : {True}

AssocTypes :: {[TypeSpec]}
  : {[]}
  | '(' CommaDelimitedTypes ')' {reverse $2}

AssocTypeDeclarations :: {[TypeParam TypeSpec]}
  : {[]}
  | '(' TypeParams_ ')' {$2}

TopLevelExpr :: {Expr}
  : StandaloneExpr {$1}
  | ConstOrVar Identifier TypeAnnotation OptionalStandaloneDefault {pe (snd $1 <+> snd $4) $ LocalVarDeclaration (fst $2) (fst $3) (fst $1) (fst $4)}
  | return TopLevelExpr {pe (snd $1 <+> pos $2) $ Return $ Just $2}
  | return ';' {pe (snd $1) $ Return $ Nothing}
  -- | defer TopLevelExpr {pe (snd $1 <+> pos $2) $ Defer $ $2}
  | continue ';' {pe (snd $1) $ Continue}
  | break ';' {pe (snd $1) $ Break}
  | yield Expr ';' {pe (snd $1 <+> pos $2) $ Yield $2}
  | tokens str {pe (snd $1 <+> snd $2) $ Tokens $ extract_lit $ fst $2}

StandaloneExpr :: {Expr}
  : ExprBlock {$1}
  | using UsingClauses StandaloneExpr {pe (snd $1 <+> snd $2) $ Using (reverse $ fst $2) $3}
  | IfStatement {$1}
  | static IfStatement {pe (snd $1 <+> pos $2) $ StaticExpr $2}
  | for Identifier in Expr ExprBlock {pe (snd $1 <+> pos $5) $ For (pe (snd $2) (Identifier (fst $2))) $4 $5}
  | while Expr ExprBlock {pe (snd $1 <+> pos $3) $ While $2 $3 False}
  | do ExprBlock while Expr ';' {pe (snd $1 <+> pos $4) $ While $4 $2 True}
  | match Expr '{' MatchCases DefaultMatchCase '}' {pe (snd $1 <+> snd $6) $ Match $2 (reverse $4) $5}
  | Expr ';' {me (pos $1) $1}

IfStatement :: {Expr}
  : if BinopTermOr ExprBlock {pe (snd $1 <+> pos $3) $ If $2 $3 (Nothing)}
  | if BinopTermOr ExprBlock else StandaloneExpr {pe (snd $1 <+> pos $5) $ If $2 $3 (Just $5)}

UsingClauses :: {([UsingType Expr TypeSpec], Span)}
  : UsingClause {([fst $1], snd $1)}
  | UsingClauses ',' UsingClause {(fst $3 : fst $1, snd $1 <+> snd $3)}

UsingClause :: {(UsingType Expr TypeSpec, Span)}
  : rules TypePath {(UsingRuleSet $ TypeSpec (fst $2) [] (snd $2), snd $1 <+> snd $2)}
  | implicit Expr {(UsingImplicit $ $2, snd $1 <+> pos $2)}

FunctionDefinition :: {FunctionDefinition Expr TypeSpec}
  : MetaMods function FunctionDefinitionBase {
    $3 {
      functionMeta = reverse $ metas $1,
      functionModifiers = reverse $ mods $1,
      functionPos = functionPos $3 <+> snd $2
    }
  }

FunctionDefinitionBase :: {FunctionDefinition Expr TypeSpec}
  : identifier TypeParams '(' VarArgs ')' TypeAnnotation OptionalBody {
    newFunctionDefinition {
      functionName = ns $ extract_identifier $1,
      functionParams = fst $2,
      functionType = fst $6,
      functionArgs = reverse (fst $4),
      functionBody = fst $7,
      functionVararg = snd $4,
      functionPos = snd $1
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
  | '{' TopLevelExprs '}' {pe (snd $1 <+> snd $3) $ Block $ reverse $2}

TopLevelExprs :: {[Expr]}
  : {[]}
  | TopLevelExprs TopLevelExpr {$2 : $1}

ModulePath :: {([Str], Span)}
  : identifier {([extract_identifier $1], snd $1)}
  | ModulePath '.' identifier {(extract_identifier $3 : fst $1, snd $1 <+> snd $3)}

CallArgs :: {[Expr]}
  : {[]}
  | Expr {[$1]}
  | CallArgs ',' Expr {$3 : $1}

MetaArg :: {MetaArg}
  : Term {MetaLiteral $ fst $ fst $1}
  | UpperOrLowerIdentifier {MetaIdentifier $ fst $1}

MetaArgs :: {[MetaArg]}
  : MetaArg {[$1]}
  | MetaArgs ',' MetaArg {$3 : $1}

Metadata :: {(Metadata, Span)}
  : "#[" identifier '(' MetaArgs ')' ']' {(Metadata {metaName = extract_identifier $2, metaArgs = reverse $4}, (snd $1) <+> (snd $6))}
  | "#[" identifier ']' {(Metadata {metaName = extract_identifier $2, metaArgs = []}, (snd $1) <+> (snd $3))}
  | "#[" ReservedIdentifier ']' {(Metadata {metaName = $2, metaArgs = []}, (snd $1) <+> (snd $3))}

ReservedIdentifier :: {Str}
  : static {"static"}

MetaMods :: {(([Metadata], [Modifier]), Span)}
  : {(([], []), NoPos)}
  | MetaMods Metadata {let (meta, mods) = fst $1 in (((fst $2 : meta, mods)), snd $1 <+> snd $2)}
  | MetaMods public {let (meta, mods) = fst $1 in (((meta, Public : mods)), snd $1 <+> snd $2)}
  | MetaMods private {let (meta, mods) = fst $1 in (((meta, Private : mods)), snd $1 <+> snd $2)}
  | MetaMods inline {let (meta, mods) = fst $1 in (((meta, Inline : mods)), snd $1 <+> snd $2)}
  | MetaMods static {let (meta, mods) = fst $1 in (((meta, Static : mods)), snd $1 <+> snd $2)}

TypeAnnotation :: {(TypeSpec, Span)}
  : {(InferredType NoPos, NoPos)}
  | ':' TypeSpec {(fst $2, snd $1 <+> snd $2)}

TypeSpec :: {(TypeSpec, Span)}
  : TypePath TypeSpecParams {(if (fst $1) == ([], "Ptr") && length (fst $2) == 1 then PointerTypeSpec (head $ fst $2) (snd $1 <+> snd $2) else TypeSpec (fst $1) (fst $2) (snd $1 <+> snd $2), snd $1 <+> snd $2)}
  | '&' TypeSpec {(PointerTypeSpec (fst $2) (snd $1 <+> snd $2), snd $1 <+> snd $2)}
  | function FunctionTypeSpec {(fst $2, snd $2)}
  | '(' TypeSpec ',' CommaDelimitedTypes ')' {let p = snd $1 <+> snd $5 in (TupleTypeSpec ((fst $2) : (reverse $4)) p, p)}
  | Term {(ConstantTypeSpec (fst $ fst $1) (snd $1), snd $1)}
  | Self {(TypeSpec ([], B.pack "Self") [] (snd $1), snd $1)}
  | '_' {(InferredType $ snd $1, snd $1)}

FunctionTypeSpec :: {(TypeSpec, Span)}
  : '(' ')' "->" TypeSpec {let p = snd $1 <+> snd $4 in (FunctionTypeSpec (fst $4) [] Nothing p, p)}
  | '(' CommaDelimitedTypes ',' identifier "..." ')' "->" TypeSpec {let p = snd $1 <+> snd $7 in (FunctionTypeSpec (fst $8) (reverse $2) (Just $ extract_identifier $4) p, p)}
  | '(' CommaDelimitedTypes ')' "->" TypeSpec {let p = snd $1 <+> snd $5 in (FunctionTypeSpec (fst $5) (reverse $2) Nothing p, p)}

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

TypeParams :: {([TypeParam TypeSpec], Span)}
  : {([], NoPos)}
  | '[' TypeParams_ ']' {(reverse $2, snd $1 <+> snd $3)}

TypeParams_ :: {[TypeParam TypeSpec]}
  : TypeParam {[$1]}
  | TypeParams_ ',' TypeParam {$3 : $1}

TypeParam :: {TypeParam TypeSpec}
  : TypeParam '=' TypeSpec {$1 {typeParamDefault = Just $ fst $3}}
  | upper_identifier TypeConstraints {(makeTypeParam (extract_upper_identifier $1)) {constraints = $2, typeParamIsConstant = False, typeParamPos = snd $1}}
  | macro_identifier {(makeTypeParam (extract_macro_identifier $1)) {constraints = [], typeParamIsConstant = True, typeParamPos = snd $1}}

TypeSpecParams :: {([TypeSpec], Span)}
  : {([], NoPos)}
  | '[' TypeSpecParams_ ']' {(reverse $2, snd $1 <+> snd $3)}

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
  : TypePath '.' UpperOrLowerIdentifier {(subPath (fst $1) (fst $3), snd $1 <+> snd $3)}
  | UpperOrLowerIdentifier {(([], fst $1), snd $1)}

ConstOrVar :: {(Bool, Span)}
  : var { (False, snd $1) }
  | const { (True, snd $1) }

VarDefinition :: {VarDefinition Expr TypeSpec}
  : MetaMods ConstOrVar UpperOrLowerIdentifier TypeAnnotation OptionalDefault ';' {
    newVarDefinition {
      varName = ns $ fst $ $3,
      varMeta = reverse (metas $1),
      varType = fst $4,
      varModifiers = reverse (mods $1),
      varDefault = $5,
      varPos = snd $2 <+> snd $3,
      varIsConst = fst $2
    }
  }

VarDefinitions :: {[VarDefinition Expr TypeSpec]}
  : {[]}
  | VarDefinitions VarDefinition {$2 : $1}

VarBlock :: {[VarDefinition Expr TypeSpec]}
  : MetaMods '{' VarDefinitions '}' {
    [v {varMeta = varMeta v ++ metas $1, varModifiers = varModifiers v ++ mods $1} | v <- $3]
  }

OptionalStandaloneDefault :: {(Maybe Expr, Span)}
  : ';' {(Nothing, snd $1)}
  | '=' undefined ';' {(Just $ pe (snd $1) Undefined, snd $1)}
  | '=' StandaloneExpr {(Just $2, snd $1 <+> pos $2)}

OptionalDefault :: {Maybe Expr}
  : {Nothing}
  | '=' undefined {Just $ pe (snd $1) Undefined}
  | '=' Expr {Just $2}

EnumVariant :: {EnumVariant Expr TypeSpec}
  : MetaMods upper_identifier ';' {
      newEnumVariant {
        variantName = ([], extract_upper_identifier $2),
        variantMeta = reverse $ metas $1,
        variantModifiers = reverse $ mods $1,
        variantArgs = [],
        variantValue = Nothing,
        variantPos = snd $2
      }
    }
  -- | MetaMods upper_identifier '=' Expr ';' {
  --     newEnumVariant {
  --       variantName = ([], extract_upper_identifier $2),
  --       variantMeta = reverse $ metas $1,
  --       variantModifiers = reverse $ mods $1,
  --       variantArgs = [],
  --       variantValue = Just $4,
  --       variantPos = snd $2
  --     }
  --   }
  | MetaMods upper_identifier '(' Args ')' ';' {
      newEnumVariant {
        variantName = ([], extract_upper_identifier $2),
        variantMeta = reverse $ metas $1,
        variantModifiers = reverse $ mods $1,
        variantArgs = reverse $4,
        variantValue = Nothing,
        variantPos = snd $2
      }
    }

VarArgs :: {([ArgSpec Expr TypeSpec], Maybe Str)}
  : Args ',' identifier "..." {($1, Just $ extract_identifier $3)}
  | Args {($1, Nothing)}

Args :: {[ArgSpec Expr TypeSpec]}
  : {[]}
  | ArgSpec {[$1]}
  | Args ',' ArgSpec {$3 : $1}

ArgSpec  :: {ArgSpec Expr TypeSpec}
  : identifier TypeAnnotation OptionalDefault {ArgSpec {
    argName = extract_identifier $1,
     argType = fst $2,
     argDefault = $3,
     argPos = snd $1 <+> snd $2 <+> (case $3 of { Just x -> pos x; Nothing -> NoPos })
    }
  }

DefinitionBody :: {Maybe (TypeSpec -> SyntacticStatement)}
  : ';' {Nothing}
  | '{' DefStatements '}' {Just (\x -> ps (snd $1 <+> snd $3) $ ExtendDefinition x $ reverse $2)}

DefStatements :: {[DefStatement Expr TypeSpec]}
  : {[]}
  | DefStatements RewriteRule {(DefRule $2) : $1}
  | DefStatements RuleBlock {(map DefRule $2) ++ $1}
  | DefStatements FunctionDefinition {(DefMethod $2) : $1}
  | DefStatements VarDefinition {(DefField $2) : $1}
  | DefStatements VarBlock {(map DefField $2) ++ $1}
  | DefStatements EnumVariant {(DefVariant $2) : $1}

RewriteExpr :: {Expr}
  : Expr {$1}
  | StandaloneExpr {$1}

RewriteRule :: {RewriteRule Expr TypeSpec}
  : rule '(' RewriteExpr ')' OptionalRuleBody {
    newRewriteRule {
      rulePattern = $3,
      ruleBody = $5,
      rulePos = snd $1 <+> snd $4
    }
  }

RuleBlock :: {[RewriteRule Expr TypeSpec]}
  : rules '{' ShortRules '}' {$3}

ShortRules :: {[RewriteRule Expr TypeSpec]}
  : {[]}
  | ShortRules ShortRule {$2 : $1}

ShortRule :: {RewriteRule Expr TypeSpec}
  : '(' RewriteExpr ')' OptionalRuleBody {
    newRewriteRule {
      rulePattern = $2,
      ruleBody = $4,
      rulePos = snd $1 <+> snd $3
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
  : if BinopTermTernary then BinopTermTernary else BinopTermTernary {pe (snd $1 <+> pos $6) $ If $2 $4 (Just $6)}
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
  : CastExpr {$1}
  | BinopTermCustom custom_op CastExpr {pe (pos $1 <+> pos $3) $ Binop (Custom $ extract_custom_op $2) $1 $3}

CastExpr :: {Expr}
  : CastExpr as TypeSpec {pe (pos $1 <+> snd $3) $ Cast $1 (fst $3)}
  | Unop {$1}

Unop :: {Expr}
  : "++" VecExpr {pe (snd $1 <+> pos $2) $ PreUnop Inc $2}
  | "--" VecExpr {pe (snd $1 <+> pos $2) $ PreUnop Dec $2}
  | '!' VecExpr {pe (snd $1 <+> pos $2) $ PreUnop Invert $2}
  | '-' VecExpr {pe (snd $1 <+> pos $2) $ PreUnop Sub $2}
  | '~' VecExpr {pe (snd $1 <+> pos $2) $ PreUnop InvertBits $2}
  | '&' VecExpr {pe (snd $1 <+> pos $2) $ PreUnop Ref $2}
  | '*' VecExpr {pe (snd $1 <+> pos $2) $ PreUnop Deref $2}
  | VecExpr "++" {pe (pos $1 <+> snd $2) $ PostUnop Inc $1}
  | VecExpr "--" {pe (pos $1 <+> snd $2) $ PostUnop Dec $1}
  | VecExpr {$1}

VecExpr :: {Expr}
  : '[' ArrayElems ']' {pe (snd $1 <+> snd $3) $ ArrayLiteral $ reverse $2}
  | '[' ArrayElems ',' ']' {pe (snd $1 <+> snd $4) $ ArrayLiteral $ reverse $2}
  | '[' ']' { pe (snd $1 <+> snd $2) $ ArrayLiteral []}
  | ArrayAccessCallFieldExpr {$1}

ArrayElems :: {[Expr]}
  : Expr {[$1]}
  | ArrayElems ',' Expr {$3 : $1}

ArrayAccessCallFieldExpr :: {Expr}
  : ArrayAccessCallFieldExpr '[' ArrayElems ']' {foldr (\x acc -> pe (pos $1 <+> snd $4) $ ArrayAccess acc x) $1 $3}
  | ArrayAccessCallFieldExpr '(' CallArgs ')' {pe (pos $1 <+> snd $4) $ Call $1 [] (reverse $3)}
  | ArrayAccessCallFieldExpr '.' Identifier {pe (pos $1 <+> snd $3) $ Field $1 $ fst $3}
  | TypeAnnotatedExpr {$1}

TypeAnnotatedExpr :: {Expr}
  : TypeAnnotatedExpr ':' TypeSpec {pe (pos $1 <+> snd $3) $ TypeAnnotation $1 (fst $3)}
  | BaseExpr {$1}

BaseExpr :: {Expr}
  : Term {pe (snd $1) (Literal (fst $ fst $1) (snd $ fst $1))}
  | this {pe (snd $1) This}
  | Self {pe (snd $1) Self}
  | Identifier {pe (snd $1) $ Identifier (fst $1)}
  | '(' identifier "..." ')' {pe (snd $1 <+> snd $4) $ VarArgListCopy $ extract_identifier $2}
  | unsafe Expr {pe (snd $1 <+> pos $2) (Unsafe $2)}
  | sizeof TypeSpec {pe (snd $1 <+> snd $2) (SizeOf $ fst $2)}
  | '(' Expr ParenthesizedExprs ')' {if null $3 then $2 {pos = snd $1 <+> snd $4} else pe (snd $1 <+> snd $4) (TupleInit ($2 : reverse $3)) }
  | null {pe (snd $1) Null}
  | empty {pe (snd $1) Empty}
  | struct TypeSpec '{' StructInitFields '}' {pe (snd $1 <+> snd $5) $ StructInit (fst $2) $4}
  | struct TypeSpec {pe (snd $1 <+> snd $2) $ StructInit (fst $2) []}
  | union TypeSpec '{' StructInitField '}' {pe (snd $1 <+> snd $2) $ UnionInit (fst $2) $4}
  | implicit TypeSpec {pe (snd $1 <+> snd $2) $ Implicit $ fst $2}
  | inline_c TypeAnnotation {pe (snd $1 <+> snd $2) $ InlineCExpr (extract_inline_c $1) (fst $2)}
  | static Expr {pe (snd $1 <+> pos $2) (StaticExpr $2)}

ParenthesizedExprs :: {[Expr]}
  : {[]}
  | ParenthesizedExprs ',' Expr {$3 : $1}

Term :: {((ValueLiteral, TypeSpec), Span)}
  : bool {((BoolValue $ extract_bool $ fst $1, ConcreteType $ TypeBool), snd $1)}
  | str {((StringValue $ extract_lit $ fst $1, makeTypeSpec "CString"), snd $1)}
  | int {let x = extract_int_lit $ fst $1 in ((IntValue $ fst x, snd x), snd $1)}
  | float {let x = extract_float_lit $ fst $1 in ((FloatValue $ fst x, snd x), snd $1)}
  | char {((IntValue $ extract_char_lit $ fst $1, ConcreteType $ TypeChar), snd $1)}

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

Identifier :: {(Identifier TypeSpec, Span)}
  : UpperOrLowerIdentifier {(Var $ ([], fst $1), snd $1)}
  | MacroIdentifier {$1}
  | '_' {(Hole, snd $1)}

MacroIdentifier :: {(Identifier TypeSpec, Span)}
  : macro_identifier {(MacroVar (extract_macro_identifier $1) (InferredType $ snd $1), snd $1)}
  | '$' '{' UpperOrLowerIdentifier TypeAnnotation '}' {(MacroVar (fst $3) (fst $4), (snd $1 <+> snd $5))}
  | '$' '{' '_' TypeAnnotation '}' {(MacroVar "_" (fst $4), (snd $1 <+> snd $5))}

{

thenP = (>>=)
returnP = return

parseError [] = Err $ KitError $ ParseError ("Unexpected end of input") (Nothing)
parseError t = Err $ KitError $ ParseError ("Unexpected " ++ (show $ fst et)) (Just $ snd et) where et = head t

-- projections
extract_identifier (LowerIdentifier x,_) = x
extract_macro_identifier (MacroIdentifier x,_) = x
extract_upper_identifier (UpperIdentifier x,_) = x
extract_inline_c (InlineC x,_) = x
extract_bool (LiteralBool x) = x
extract_lit (LiteralString x) = x
extract_int_lit (LiteralInt x y) = (x, numSpec y)
extract_float_lit (LiteralFloat x y) = (x, numSpec y)
extract_char_lit (LiteralChar x) = x
extract_assign_op (Op (AssignOp x),_) = x
extract_custom_op (Op (Custom x),_) = x
numSpec (Just x) = TypeSpec ([], s_pack $ show x) [] NoPos
numSpec Nothing = (InferredType NoPos)

ns x = ([], x)

tc :: [Token] -> [TokenClass]
tc t = [fst t' | t' <- t]

fp :: [Span] -> Span
fp s = foldr (<+>) NoPos s

metas :: (([Metadata], [Modifier]), Span) -> [Metadata]
metas ((a,_),_) = a
mods :: (([Metadata], [Modifier]), Span) -> [Modifier]
mods ((_,b),_) = b

addImplExtension
  :: DefStatement a b -> TraitImplementation a b -> TraitImplementation a b
addImplExtension (DefField v) t =
  if elem Static (varModifiers v)
    then t { implStaticFields = v : implStaticFields t }
    else throwk $ BasicError
          "Non-static fields can't be added to a trait implementation"
          (Just $ varPos v)
addImplExtension (DefMethod f) t =
  if elem Static (functionModifiers f)
    then t { implStaticMethods = f : implStaticMethods t }
    else t { implMethods = f : implMethods t }
addImplExtension _ t =
  throwk $ BasicError "Invalid trait implementation extension" (Just $ implPos t)

}
