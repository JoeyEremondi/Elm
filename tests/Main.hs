module Main where

{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}


import Test.Framework

import Tests.Compiler
import Tests.Property
import Tests.TH

main :: IO () 
main = defaultMain [ compilerTests
                   , propertyTests
                   ]
