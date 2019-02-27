module Testcases where

import Data.String

import Syntax

float42' = Float 42
float42 = [Float 42]
floatsEndingWith42 = [Float 0, Float 42]

functionWithNoArgument' name = Function name' [] $ float42'
  where name' = fromString name :: Name

functionWithNoArgument = [functionWithNoArgument' "noArguments"]

functionCallWithNoArgument =
  [ functionWithNoArgument' calleeName
  , Call (fromString calleeName :: Name) []
  ]
  where calleeName = "test"

functionCallWithFloatArgument =
  [ Function calleeName [arg] $ arg
  , Call calleeName float42
  ]
  where
    arg         = Var (fromString "arg" :: Name)
    calleeName  = (fromString "returnFirstArg" :: Name)
