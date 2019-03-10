module Helpers where

import           LLVM
import           LLVM.IRBuilder
import           LLVM.AST.Linkage
import           LLVM.AST.Global
import           LLVM.AST.ParameterAttribute
import           LLVM.AST.Type
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Float                  as F

bool = i1
char = i8
int = i32
charptr = ptr i8

intv v = op $ C.Int 32 v
doublev v = op $ C.Float (F.Double v)

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n
local t n = AST.LocalReference t (AST.Name n)
