import           Test.Tasty
import           Test.Tasty.HUnit

import Control.Monad ((>=>))
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Bits

import Data.Char (ord)
import Data.List (elemIndex)
import Data.Monoid ((<>), mempty)

import Data.Word (Word8)

newtype Base64 = Base64 T.Text deriving (Show, Eq)
newtype Hex = Hex T.Text deriving (Show, Eq)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "Negative n"

-- TOOD: This is O(N) but should be O(1) with better implementation
hexchar2int x = elemIndex x (['0'..'9'] <> ['a' .. 'f'])

hex2bytes :: Hex -> Maybe B.ByteString
hex2bytes = hex2bytes' mempty

hex2bytes' :: [Word8] -> Hex -> Maybe B.ByteString
hex2bytes' accum (Hex cs)
  | T.length cs == 0 = Just . B.pack . reverse $ accum
  | otherwise =
    case mbByte of
      Just (byte, rest) -> hex2bytes' (byte:accum) (Hex rest)
      Nothing           -> Nothing

    where
      mbByte = do
        (msb, rest) <- T.uncons cs
        (lsb, rest) <- T.uncons rest
        msb <- hexchar2int msb
        lsb <- hexchar2int lsb

        return (fromIntegral $ msb * 16 + lsb, rest)

bytes2base64 :: B.ByteString -> Maybe Base64
bytes2base64 bs =
  let padded = B.unpack $ bs <> B.replicate (abs $ B.length bs `mod` (-3)) 0 in
  let chunked = chunksOf 3 padded in

  let encoded = map encodeChunk chunked in

  Base64 . T.concat <$> sequence encoded

  where
    -- TODO: Don't use !!
    encodeChar n = Just $ (['A'..'Z'] <> ['a' .. 'z'] <> ['0'..'9'] <> ['+', '/']) !! n
    encodeChunk [a, b, c] =
      let n = fromIntegral a `shift` 16 .|. fromIntegral b `shift` 8 .|. fromIntegral c in
      let b1 = (n .&. (63 `shift` 18)) `shiftR` 18 in
      let b2 = (n .&. (63 `shift` 12)) `shiftR` 12 in
      let b3 = (n .&. (63 `shift` 6)) `shiftR` 6 in
      let b4 = (n .&. (63 `shift` 0)) `shiftR` 0 in

      T.pack <$> sequence [encodeChar b1, encodeChar b2, encodeChar b3, encodeChar b4]

-- Get groups of 3 chars, combine into 24 bit number
-- Get groups of 6 bits
-- Convert each 6 bit number to base64 char

hex2base64 :: Hex -> Maybe Base64
hex2base64 = hex2bytes >=> bytes2base64

main :: IO ()
main = defaultMain $ testGroup "Set 1"
  [ testCase "Problem 1, simple" $
    (Just . B.pack $ [0, 255]) @=? hex2bytes (Hex "00ff")
  , testCase "Problem 1, simple" $
    (Just (Base64 "AAAA")) @=? bytes2base64 (B.pack [0])
  , testCase "Problem 1, given" $
    Just (Base64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t") @=?
      hex2base64 (Hex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  ]
