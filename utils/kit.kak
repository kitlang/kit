# http://www.kitlang.org
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Detection
# ‾‾‾‾‾‾‾‾‾

hook global BufCreate .*\.(kit) %{
    set-option buffer filetype kit
}

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾

add-highlighter shared/kit regions
add-highlighter shared/kit/code default-region group
add-highlighter shared/kit/string  region '"' (?<!\\)(\\\\)*"         fill string
add-highlighter shared/kit/comment region '//' '$'                     fill comment

add-highlighter shared/kit/code/ regex %{\b(true|false|None|Some)\b} 0:value
add-highlighter shared/kit/code/ regex \b(struct|enum|union|abstract|function|implement|specialize|trait|typedef|var|private|public|static|const|inline|using|return|throw|this|for|as|in|if|then|else|do|while|match|default|unsafe|rule|rules|implicit|sizeof|import|include|print|printf)\b 0:keyword
add-highlighter shared/kit/code/ regex \b(CArray|CString|Char|Ptr|Size|Short|Long|Byte|Float|Double|UInt8|UInt16|UInt32|UInt64|Int|Int8|Int16|Int32|Int64|Float32|Float64|)\b 0:type

# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook -group kit-highlight global WinSetOption filetype=kit %{ add-highlighter window/kit ref kit }
hook -group kit-highlight global WinSetOption filetype=(?!kit).* %{ remove-highlighter window/kit }
