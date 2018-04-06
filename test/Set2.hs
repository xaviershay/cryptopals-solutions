{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

test_Set_2_Challenge_9 = testGroup "Set 2 Challenge 9"
  [ testCase "given" $
    pkcs7pad 20 "YELLOW SUBMARINE" @=? "YELLOW SUBMARINE\x04\x04\x04\x04"
  , testCase "longer" $
    pkcs7pad 4 "AAAAA" @=? "AAAAA\x03\x03\x03"
  , testCase "empty" $
    pkcs7pad 2 "" @=? "\x02\x02"
  ]

focus = defaultMain test_Set_2_Challenge_9
