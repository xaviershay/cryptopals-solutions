{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad      ((>=>))
import           Data.Bits
import qualified Data.ByteString    as B
import           Data.Char          (ord)
import           Data.Foldable      (foldl')
import           Data.List          (elemIndex, sortOn)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromJust)
import           Data.Monoid        (mempty, (<>))
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8')
import           Data.Word          (Word8)
import           Test.Tasty
import           Test.Tasty.HUnit

newtype Base64 = Base64 T.Text deriving (Show, Eq)
newtype HexBytes = HexBytes T.Text deriving (Show, Eq)

hex2bytes :: HexBytes -> Maybe B.ByteString
hex2bytes = hex2bytes' mempty
  where
    hexmap = M.fromList $ zip (['0'..'9'] <> ['a' .. 'f']) [0..]
    hexchar2int x = M.lookup x hexmap

    hex2bytes' :: [Word8] -> HexBytes -> Maybe B.ByteString
    hex2bytes' accum (HexBytes cs)
      | T.length cs == 0 = Just . B.pack . reverse $ accum
      | otherwise = do
          (msb, rest) <- T.uncons cs
          (lsb, rest) <- T.uncons rest
          msb <- hexchar2int msb
          lsb <- hexchar2int lsb

          let byte = packWords 4 [msb, lsb]

          hex2bytes' (byte:accum) (HexBytes rest)

packWords :: (Integral a, Bits b, Num b) => Int -> [a] -> b
packWords bitsPerWord bs =
  foldl' (\x (b, i) -> x .|. fromIntegral b `shift` (bitsPerWord * i)) 0 $
  zip bs (reverse [0 .. length bs - 1])

unpackWords :: (Bits a, Num a) => Int -> Int -> a -> [a]
unpackWords numberOfWords bitsPerWord packed =
  f <$> reverse [0..numberOfWords-1]

  where
    f i =
      let mask = 2 ^ bitsPerWord - 1 in
      let pos = i * bitsPerWord in

      (packed .&. mask `shift` pos) `shiftR` pos

bytes2base64 :: B.ByteString -> Maybe Base64
bytes2base64 bs =
  let padded = B.unpack $ bs <> B.replicate (abs $ B.length bs `mod` (-3)) 0 in
  let chunked = chunksOf 3 padded in

  let encoded = map encodeChunk chunked in

  Base64 . T.concat <$> sequence encoded

  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n l
      | n > 0 = take n l : chunksOf n (drop n l)
      | otherwise = error "Negative n"

    base64map = M.fromList $
      zip [0..] (['A'..'Z'] <> ['a' .. 'z'] <> ['0'..'9'] <> ['+', '/'])

    encodeChar :: Int -> Maybe Char
    encodeChar n = M.lookup n base64map

    encodeChunk = fmap T.pack . mapM encodeChar . unpackWords 4 6 . packWords 8

hex2base64 :: HexBytes -> Maybe Base64
hex2base64 = hex2bytes >=> bytes2base64

xorBytes :: B.ByteString -> B.ByteString -> B.ByteString
xorBytes xs ys = B.pack $ B.zipWith xor xs ys

fromHexString :: T.Text -> B.ByteString
fromHexString = fromJust . hex2bytes . HexBytes

toMaybe :: Either a b -> Maybe b
toMaybe x = case x of
  Right x -> Just x
  Left _  -> Nothing

-- The input cypher is known to xor'ed with a single character. Try decoding
-- against all characters, using a simple heuristic to determine which output
-- looks most "english like".
set1challenge3 :: B.ByteString -> Maybe T.Text
set1challenge3 bs =
  let candidateKeys = [B.pack $ replicate (B.length bs) c | c <- [0..255]] in
  let candidateTexts = map (xorBytes bs) candidateKeys in

  -- head is safe here because it is operating on the list constructed with a
  -- constant range above
  toMaybe . decodeUtf8' . head . sortOn (negate . score) $ candidateTexts

  where
    -- The score is the number of simple characters in the string.
    score :: B.ByteString -> Int
    score = B.foldl' (\a b -> a + (scoreChar . fromIntegral $ b)) 0
    scoreChar b = M.findWithDefault 0 b scoreMap
    scoreMap = M.fromList $ zip
      (map ord $ ['a'..'z'] <> ['A'..'Z'] <> [' '])
      (repeat 1)

main :: IO ()
main = defaultMain $ testGroup "Set 1"
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
  ]
