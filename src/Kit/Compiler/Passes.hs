module Kit.Compiler.Passes (
  module Kit.Compiler.Passes.BuildModuleGraph,
  module Kit.Compiler.Passes.IncludeCModules,
  module Kit.Compiler.Passes.ExpandMacros,
  module Kit.Compiler.Passes.ResolveModuleTypes,
  module Kit.Compiler.Passes.FlattenTraits,
  module Kit.Compiler.Passes.TypeModuleContent,
  module Kit.Compiler.Passes.SpecializeTypes,
  module Kit.Compiler.Passes.GenerateMonomorphs,
  module Kit.Compiler.Passes.GenerateIr,
  module Kit.Compiler.Passes.GenerateCode,
  module Kit.Compiler.Passes.CompileCode
) where

import Kit.Compiler.Passes.BuildModuleGraph
import Kit.Compiler.Passes.IncludeCModules
import Kit.Compiler.Passes.ExpandMacros
import Kit.Compiler.Passes.ResolveModuleTypes
import Kit.Compiler.Passes.FlattenTraits
import Kit.Compiler.Passes.TypeModuleContent
import Kit.Compiler.Passes.SpecializeTypes
import Kit.Compiler.Passes.GenerateMonomorphs
import Kit.Compiler.Passes.GenerateIr
import Kit.Compiler.Passes.GenerateCode
import Kit.Compiler.Passes.CompileCode
