{-# LANGUAGE OverloadedStrings #-}

module AesonSpec(tests) where

import Control.Monad
import Data.Aeson
import Data.Config
import Data.Vector
import Test.HUnit

tests = TestList
    [ testBasic
    , testApp
    ]

testBasic = testConv "examples/basic.conf"
    [ "foo" .= object
        [ "port" .= Number 1234
        , "addr" .= String "localhost"
        ]
    ]

testApp = testConv "examples/app.conf"
    [ "foo" .= array [object ["baz" .= Number 42]]
    , "foo2" .= object ["baz" .= Number 42]
    , "toto" .= Bool False
    , "rawString" .= String "\n            This is a multi-\n            lines String\n            "
    , "another.string" .= String "I'm a String"
    , "one.more.string" .= String "one more string"
    , "nested" .= object
        [ "list" .= array [String "one", Number 1, String "both"]
        , "homing" .= object ["pass" .= object ["b" .= String "feez", "a" .= String "Prop"]]
        , "another" .= array [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6]
        ]
    ]

-- test HOCON to Aeson JSON conversion
testConv file kvs = TestCase $ do
    conf <- loadConfig file
    let obtained = toJSON conf
    let expected = object kvs
    assertEqual "" expected obtained

array :: [Value] -> Value
array = Array . fromList
