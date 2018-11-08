" Vim syntax file
"
" Language: Kit
" Last Change: 2018-11-08

if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "kit"


syn region kitCommentOneLine start="//" end="$"
syn region kitCommentMultiLine start="/\*" end="\*/"

syn keyword kitConstant null empty this Self Some None Cons Empty
syn region kitStringOneLine start=/"/ skip=/\\"/ end=/"/ oneline
syn region kitStringMultiLine start=/"""/ end=/"""/
syn match kitCharacter /c'.'/
" syn match kitNumber ...
syn keyword kitBoolean true false
" syn match kitFloat ...

" syn match kitIdentifier ...
" syn match kitFunction ...

" syn keyword kitStatement ...
syn keyword kitConditional if then else match
syn keyword kitRepeat for do while
syn keyword kitLabel default
syn keyword kitOperator sizeof
syn keyword kitKeyword function trait implement return as break defer
syn keyword kitException throw

" syn keyword kitPreProc ...
syn keyword kitInclude include import
" syn keyword kitDefine ...
syn keyword kitMacro rules using rule implicit
" syn keyword kitPreCondit ...

syn keyword kitType Int8 Int16 Short Int32 Int64 Long
syn keyword kitType Uint8 Byte Uint16 Unt32 Uint64
syn keyword kitType Float32 Float Float64 Double
syn keyword kitType Void Bool Char Int Size Ptr
syn keyword kitType Numeric Integral NumericMixed
syn keyword kitType List CArray CString Box
syn keyword kitType Iterable Iterator Option
syn keyword kitType Allocator LinearAllocator StackAllocator
syn keyword kitType Path
syn keyword kitStorageClass public static
syn keyword kitStorageClass var const
" syn keyword kitStorageClass register volatile
syn keyword kitStructure struct union enum abstract
syn keyword kitTypedef typedef specialize

" syn keyword kitSpecial
" syn keyword kitSpecialChar
" syn keyword kitTag
" syn keyword kitDelimiter
" syn keyword kitSpecialComment
" syn keyword kitDebug

" syn keyword kitUnderlined
" syn keyword kitIgnore
" syn keyword kitError
" syn keyword kitTodo


hi def link kitCommentOneLine Comment
hi def link kitCommentMultiLine Comment

hi def link kitConstant Constant
hi def link kitStringOneLine String
hi def link kitStringMultiLine String
hi def link kitCharacter Character
" hi def link kitNumber Number
hi def link kitBoolean Boolean
" hi def link kitFloat Float

" hi def link kitIdentifier Identifier
" hi def link kitFunction Function

" hi def link kitStatement Statement
hi def link kitConditional Conditional
hi def link kitRepeat Repeat
hi def link kitLabel Label
hi def link kitOperator Operator
hi def link kitKeyword Keyword
hi def link kitException Exception

" hi def link kitPreProc Preproc
hi def link kitInclude Include
" hi def link kitDefine Define
hi def link kitMacro Macro
" hi def link kitPreCondit PreCondit

hi def link kitType Type
hi def link kitStorageClass StorageClass
hi def link kitStructure Structure
hi def link kitTypedef Typedef

" hi def link kitSpecial Special
" hi def link kitSpecialChar SpecialChar
" hi def link kitTag Tag
" hi def link kitDelimiter Delimiter
" hi def link kitSpecialComment SpecialComment
" hi def link kitDebug Debug

" hi def link kitUnderlined Underlined
" hi def link kitIgnore Ignore
" hi def link kitError Error
" hi def link kitTodo Todo
