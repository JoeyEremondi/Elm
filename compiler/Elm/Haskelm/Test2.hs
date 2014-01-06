module Elm.Haskelm.Test2 where

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Elm.Haskelm.Quasi
import Elm.Haskelm.EToH
import Elm.Haskelm.Test

-- $(translate theTest)

-- $(translate theTest2)

$(decHaskAndElm "myGreatElmString"
    [d|
        data Foo = Baz Int | Bar String

        unFoo x = case x of
            Baz x -> Bar "3"
            Bar y -> Baz 4

        x :: Int
        x = 3 + 4 - 5 * 67 `mod` 8

        y :: String -> Int
        y _ = if 3 < 4 then 5 else 6

        z :: Int -> Int
        z arg = if
            |arg < 3 -> 3
            | arg == 3 -> 4
            | arg < 100 -> 99
            |otherwise -> 10

        type MyInt = Int
    |])
