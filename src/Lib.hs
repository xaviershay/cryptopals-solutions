module Lib where

import           Control.Monad           ((>=>), join, mapM)
import           Data.Bits
import qualified Data.ByteString.Lazy    as B
import           Data.Char               (ord)
import           Data.Foldable           (foldl')
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromJust)
import           Data.Monoid             (mempty, (<>))
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8')
import qualified Data.Tuple              as Tuple

newtype Base64 = Base64 T.Text deriving (Show, Eq)
newtype HexBytes = HexBytes T.Text deriving (Show, Eq)

hex2bytes :: HexBytes -> Maybe B.ByteString
hex2bytes = hex2bytes' mempty
  where
    hexmap = M.fromList $ zip (['0'..'9'] <> ['a' .. 'f']) [0..]
    hexchar2int x = M.lookup x hexmap

    hex2bytes' :: [Int] -> HexBytes -> Maybe B.ByteString
    hex2bytes' accum (HexBytes cs)
      | T.length cs == 0 = Just . B.pack . map fromIntegral . reverse $ accum
      | otherwise = do
          (msb, rest) <- T.uncons cs
          (lsb, rest) <- T.uncons rest
          msb <- hexchar2int msb
          lsb <- hexchar2int lsb

          let byte = packWords 4 [msb, lsb]

          hex2bytes' (byte:accum) (HexBytes rest)

readBase64File :: String -> IO B.ByteString
readBase64File path = do
  encodedText <- concat . lines <$> readFile path

  return . fromJust . base642bytes . Base64 . T.pack $ encodedText


packWords :: Int -> [Int] -> Int
packWords bitsPerWord bs =
  foldl' (\x (b, i) -> x .|. fromIntegral b `shift` (bitsPerWord * i)) 0 $
  zip bs (reverse [0 .. length bs - 1])

unpackWords :: Int -> Int -> Int -> [Int]
unpackWords numberOfWords bitsPerWord packed =
  f <$> reverse [0..numberOfWords-1]

  where
    f i =
      let mask = 2 ^ bitsPerWord - 1 in
      let pos = i * bitsPerWord in

      (packed .&. mask `shift` pos) `shiftR` pos

base642bytes :: Base64 -> Maybe B.ByteString
base642bytes (Base64 chars) = do
  --traceM (show (chars))
  --traceM . show $ map decodeChar . T.unpack $ chars
  decoded <- mapM decodeChar . T.unpack $ chars

  let chunks = chunksOf 4 decoded :: [[Int]]
  --traceM . show $ chars
  --traceM . show $ decoded
  --traceM . show $ base64rmap
  let bytes = concatMap packBytes chunks :: [Int]

  --traceM . show . map (printf "%08b" :: Int -> String) $ bytes
  return . B.pack . map fromIntegral $ bytes

  where
    decodeChar :: Char -> Maybe Int
    decodeChar c = M.lookup c base64rmap

    packBytes ::  [Int] -> [Int]
    packBytes = unpackWords 3 8 . packWords 6

base64map :: M.Map Int Char
base64map = M.fromList $
  zip [0..] (['A'..'Z'] <> ['a' .. 'z'] <> ['0'..'9'] <> ['+', '/'])

base64rmap :: M.Map Char Int
base64rmap = M.insert '=' 0 $ M.fromList . map Tuple.swap . M.toList $ base64map

hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance a b = sum . map bitDifference $ B.zip a b
  where
    bitDifference (x, y) = numberOfSetBits $ xor x y

    -- This implementation may be surprising but try it by hand with a couple
    -- of inputs to see how it works. It's neat!
    numberOfSetBits 0 = 0
    numberOfSetBits z = 1 + numberOfSetBits (z .&. (z - 1))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = take n l : chunksOf n (drop n l)
  | otherwise = error "Negative n"

bytes2base64 :: B.ByteString -> Maybe Base64
bytes2base64 bs =
  let padded = B.unpack $ bs <> B.replicate (abs $ B.length bs `mod` (-3)) 0 in
  let chunked = chunksOf 3 padded in

  let encoded = map (encodeChunk . map fromIntegral) chunked in

  Base64 . T.concat <$> sequence encoded

  where

    encodeChar :: Int -> Maybe Char
    encodeChar n = M.lookup n base64map

    encodeChunk = fmap T.pack . mapM encodeChar . unpackWords 4 6 . packWords 8

hex2base64 :: HexBytes -> Maybe Base64
hex2base64 = hex2bytes >=> bytes2base64

xorBytes :: B.ByteString -> B.ByteString -> B.ByteString
xorBytes xs ys =
  let longestLength = max (B.length xs) (B.length ys) in

  B.take longestLength $ B.pack (B.zipWith xor (B.cycle xs) (B.cycle ys))

fromHexString :: T.Text -> B.ByteString
fromHexString = fromJust . hex2bytes . HexBytes

-- A heuristic to detect the "english-ness" of some bytes. The higher the
-- return value, the more likely. Normalized by length of input.
score :: B.ByteString -> Double
score bytes =
  B.foldl' (\a b -> a + (scoreChar . fromIntegral $ b)) 0 bytes
  `realDiv`
  B.length bytes

scoreChar b = M.findWithDefault 0 b scoreMap
scoreMap = M.fromList $ zip
  (map ord $ ['a'..'z'] <> ['A'..'Z'] <> [' '])
  (repeat 1)

-- Given a byte string, return all possible single-char repeating keys
generateSingleCharKeys :: B.ByteString -> [B.ByteString]
generateSingleCharKeys bs = [B.replicate (B.length bs) c | c <- [0..255]]

-- Given possible UTF8 bytes, return the one that is actually UTF8 and most
-- likely to be valid english. This is a simple heuristic method, so could be
-- wrong! Returns nothing if no input is valid UTF8.
chooseMostLikelyText :: [B.ByteString] -> Maybe T.Text
chooseMostLikelyText =
  -- Decoding UTF8 can potentially fail, so we end up with a nested Maybe,
  -- which the join removes.
  join . fmap (toMaybe . decodeUtf8') .

  -- Use the first text that scores higher than an arbitrary threshold.
  headMaybe . filter (\x -> score x >= 0.9)

-- Generic downcasting of Either to Maybe
toMaybe :: Either a b -> Maybe b
toMaybe x = case x of
  Right x -> Just x
  Left _  -> Nothing

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

-- Allows us to freely divide integers and doubles without needing to worry
-- about resulting types or integer truncation.
realDiv :: (Real a, Real b, Fractional c) => a -> b -> c
realDiv x y = realToFrac x / realToFrac y
