module Lib where

import           Control.Monad      ((>=>))
import           Data.Bits
import qualified Data.ByteString    as B
import           Data.Char          (ord)
import           Data.Foldable      (foldl')
import           Data.List          (sortOn)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromJust)
import           Data.Monoid        (mempty, (<>))
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8')
import           Data.Word          (Word8)

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

-- A heuristic to detect the "english-ness" of some bytes. The higher the
-- return value, the more likely.
score :: B.ByteString -> Int
score = B.foldl' (\a b -> a + (scoreChar . fromIntegral $ b)) 0
scoreChar b = M.findWithDefault 0 b scoreMap
scoreMap = M.fromList $ zip
  (map ord $ ['a'..'z'] <> ['A'..'Z'] <> [' '])
  (repeat 1)

-- Given a byte string, return all possible single-char repeating keys
generateSingleCharKeys :: B.ByteString -> [B.ByteString]
generateSingleCharKeys bs =
  [B.pack $ replicate (B.length bs) c | c <- [0..255]]

-- Given possible UTF8 bytes, return the one that is actually UTF8 and most
-- likely to be valid english. This is a simple heuristic method, so could be
-- wrong! Returns nothing if no input is valid UTF8.
chooseMostLikelyText :: [B.ByteString] -> Maybe T.Text
chooseMostLikelyText bs =
  let sorted = sortOn (negate . score) bs in

  case sorted of
    []     -> Nothing
    (x:_) -> toMaybe . decodeUtf8' $ x
