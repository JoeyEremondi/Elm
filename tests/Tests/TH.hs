

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-} 
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

module Tests.TH where

import Language.Elm.TH
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (Assertion, assertFailure, assertBool)


-- $(translate theTest)

-- $(translate theTest2)

$(decHaskAndElm "myGreatElmString"
    [d|
        data Foo = Baz [Int] | Bar String | Biz (Int, Int, String)

        data Foo2 = TheBar | TheBaz
        
        unFoo x = case x of
            Baz x -> Bar ret
            Bar y -> Baz [4]
            where
              ret = "3"

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

$(decsFromString "elmFromString" "data X = Y | Z" )

$(decsFromFile "elmFromFile" "tests/test-files/test.hst" )

