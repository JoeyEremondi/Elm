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

$(decHaskAndElm [d| data Foo = Baz Int | Bar String |])
