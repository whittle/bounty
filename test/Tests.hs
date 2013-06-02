{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import qualified Tests.Parser

main :: IO ()
main = defaultMain [ Tests.Parser.tests ]
