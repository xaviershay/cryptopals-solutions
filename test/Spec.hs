{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy    as B
import           Data.Monoid        ((<>))
import qualified Data.Text.Lazy          as T

import           Test.Tasty
import           Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  set1challenge4data <- lines <$> readFile "data/4.txt"

  defaultMain $ testGroup "Set 1"
    [ testGroup "Challenge 1"
      [ testCase "simple hex2bytes" $
        (Just . B.pack $ [0, 255]) @=? hex2bytes (HexBytes "00ff")
      , testCase "simple bytes2base64" $
        Just (Base64 "AAAA") @=? bytes2base64 (B.pack [0])
      , testCase "invalid hex char"   $ Nothing @=? hex2base64 (HexBytes "ZZ")
      , testCase "invalid hex length" $ Nothing @=? hex2base64 (HexBytes "F")
      , testCase "given" $
        Just (Base64 $ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiB" <>
                       "saWtlIGEgcG9pc29ub3VzIG11c2hyb29t") @=?
        hex2base64
          (HexBytes $ "49276d206b696c6c696e6720796f757220627261696e2" <>
                      "06c696b65206120706f69736f6e6f7573206d757368726f6f6d")
      ]
    , testGroup "Challenge 2"
      [ testCase "given" $
        fromHexString "746865206b696420646f6e277420706c6179" @=?
          fromHexString "1c0111001f010100061a024b53535009181c" `xorBytes`
          fromHexString "686974207468652062756c6c277320657965"
      ]
    , testGroup "Challenge 3"
      [ testCase "given" $
        Just "Cooking MC's like a pound of bacon" @=? set1challenge3
          (fromHexString $ "1b37373331363f78151b7f2b783431333d7839782" <>
                           "8372d363c78373e783a393b3736")
      ]
    , testGroup "Challenge 4"
      [ testCase "given" $
        Just "Now that the party is jumping\n" @=?
          set1challenge4 set1challenge4data
      ]
    , testGroup "Challenge 5"
      [ testCase "given" $
        (fromHexString $ "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623" <>
                         "d63343c2a26226324272765272a282b2f20430a652e2c652a" <>
                         "3124333a653e2b2027630c692b20283165286326302e27282f")
          @=?
            xorBytes "ICE"
              ("Burning 'em, if you ain't quick and nimble\n" <>
              "I go crazy when I hear a cymbal")
      ]

    ]

-- The input cypher is known to xor'ed with a single character. Try decoding
-- against all characters, using a simple heuristic to determine which output
-- looks most "english like".
set1challenge3 :: B.ByteString -> Maybe T.Text
set1challenge3 bs =
  let candidateKeys  = generateSingleCharKeys bs in
  let candidateTexts = map (xorBytes bs) candidateKeys in

  -- head is safe here because it is operating on the list constructed with a
  -- constant range above
  chooseMostLikelyText candidateTexts

set1challenge4 :: [String] -> Maybe T.Text
set1challenge4 ss =
  let candidateInputs = map (fromHexString . T.pack) ss in
  let candidateTexts =
        [ xorBytes x y
           | x <- candidateInputs
           , y <- generateSingleCharKeys x
        ] in

  chooseMostLikelyText candidateTexts
