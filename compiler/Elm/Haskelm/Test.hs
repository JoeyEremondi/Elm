module Elm.Haskelm.Test where

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Elm.Haskelm.Quasi
import Elm.Haskelm.EToH

import SourceSyntax.Declaration


theTest :: [Declaration () ()]

theTest = [elm|data Foo = Bar | Baz |]

theTest2 :: [Declaration () ()]
theTest2 = $(elmFile "test.elm")