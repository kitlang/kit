module Kit.Ast.ModulePath where

import Data.List
import System.FilePath
import Kit.Str

type ModulePath = [Str]

parseModulePath :: Str -> ModulePath
parseModulePath s = s_split '.' s

showModulePath :: ModulePath -> Str
showModulePath s = s_join "." s

moduleFilePath :: ModulePath -> FilePath
moduleFilePath mod = replaceExtension (joinPath (map s_unpack mod)) ".kit"
