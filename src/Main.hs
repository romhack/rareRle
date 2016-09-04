module Main where
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy as Bs
import           Data.Int
import           Data.List
import           Data.Word
import           Numeric              (showHex)
import           System.Environment
import           Text.Printf

data RleEntry = Raw Word8
                | FillByte {len :: Int, fillByte :: Word8} --standard RLE
                | FillWord {len :: Int, fillWord :: Word16}
                | InterleaveByte {byte :: Word8, block :: [Word8]} --const hi byte
                | InterleaveHi {hiNybble :: Word8, loNybbles :: [Word8]} --const hi nybble
                | InterleaveLo {loNybble :: Word8, hiNybbles :: [Word8]} -- const lo nybble


instance Show RleEntry where
  show (Raw b) = printf "0x%02X" b
  show (FillByte l b) = printf "{FillByte %d 0x%02X}" l b
  show (FillWord l w) = printf "{FillWord %d 0x%04X}" l w
  show (InterleaveByte b bs) = printf "{InterleaveByte %02X " b ++ listToHex bs ++ "}"
  show (InterleaveHi n ns) = printf "{InterleaveHi %02X " n ++ listToHex ns ++ "}"
  show (InterleaveLo n ns) = printf "{InterleaveLo %02X " n ++ listToHex ns ++ "}"

listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]"

bitsToNum :: (Num a) => [Bool] -> a
bitsToNum = foldl' (\byte b -> byte*2 + if b then 1 else 0) 0

numToBits :: (Bits a, Num a) => Int -> a -> [Bool] --number with a given bitfield width to bits list
numToBits n b = map (testBit b) [n-1, n-2..0]

getEntries :: Get [RleEntry]
getEntries = do
  stopByte <- getWord8 --read stopByte initially
  go stopByte
  where
    go :: Word8 -> Get [RleEntry]
    go stop = do
      copyByte <- getWord8
      if copyByte /= stop
        then do --stopByte not reached - raw copy
          rest <- go stop
          return $ Raw copyByte : rest
        else  do --it will be RLE code
          command <- getWord8
          case numToBits 8 command of

            (True:True:False:False:False:False:rest) -> return [] --exit

            (False:True:count) -> do --fill FF count
              rest <- go stop
              return $ FillByte (bitsToNum count) 0xFF : rest

            (False:False:count) -> do --fill 00 count
              rest <- go stop
              return $ FillByte (bitsToNum count) 0 : rest

            (True:False:count) -> do --fill 00FF count
              rest <- go stop
              return $ FillWord (bitsToNum count) 0x00FF : rest

            (True:True:False:True:count) -> do
              fillWord <- getWord16be --RLE word16
              rest <- go stop
              return $ FillWord (bitsToNum count) fillWord : rest

            (True:True:False:False:False:True:rest) -> do
              newStopByte <- getWord8 --get new stopByte
              go newStopByte

            (True:True:False:False:True:count) -> do
              let count' = bitsToNum count + 3-- interleave with const hiByte as 0
              block <- getLazyByteString count'
              rest <- go stop
              return $ InterleaveByte 0 (Bs.unpack block) : rest

            (True:True:True:False:lNybble) -> do
              let lNybble' = bitsToNum lNybble --interleave with const loNybble
              byte <- getWord8
              let
                count = (byte `shiftR` 4) + 7
                hNybble = byte .&. 0xF
              block <- getLazyByteString $ fromIntegral (count `shiftR` 1)
              let nybbles = take (fromIntegral count) $ hNybble : getNybbles block
              rest <- go stop
              return $ InterleaveLo lNybble' nybbles : rest

            (True:True:True:True:hNybble) -> do
              let hNybble' = bitsToNum hNybble --interleave with const hiNybble
              byte <- getWord8
              let
                count = (byte `shiftR` 4) + 7
                lNybble = byte .&. 0xF
              block <- getLazyByteString $ fromIntegral (count `shiftR` 1)
              let nybbles = take (fromIntegral count) $ lNybble : getNybbles block
              rest <- go stop
              return $ InterleaveHi hNybble' nybbles : rest

            _ -> error "unknown RLE command"

getNybbles :: Bs.ByteString -> [Word8] --return list of hi-lo nyblles from bytestring
getNybbles bs = concatMap separate $ Bs.unpack bs
  where separate x = [x `shiftR` 4, x .&. 0xF]

serializeEntry :: RleEntry -> [Word8]
serializeEntry (Raw x) = [x]
serializeEntry (FillByte len val) = replicate len val
serializeEntry (FillWord len val) = map fromIntegral $ concat $ replicate len [val `shiftR` 8, val .&. 0xFF]
serializeEntry (InterleaveByte val block) = concatMap (\x -> [val, x]) block
serializeEntry (InterleaveHi hNyb lNybs) = map (\lo -> (hNyb `shiftL` 4) .|. lo) lNybs
serializeEntry (InterleaveLo lNyb hNybs) = map (\hi -> (hi `shiftL` 4) .|. lNyb) hNybs




main :: IO ()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "gbc2Tga tool 0.1"
    parse [name, offset] = go name offset
    parse _ = putStrLn "Provide one ROM name and one data offset"

go name offsetStr = do
  rom <- Bs.readFile name
  let
    gfxOffset = read offsetStr
    entriesResult = runGetOrFail getEntries $ Bs.drop gfxOffset rom
  case entriesResult of
    Left (_, _, err) -> putStrLn err
    Right (_, readCount, entries) -> do
        let
          out = concatMap serializeEntry entries
          outLen = length out
          compressRatio :: Float
          compressRatio = fromIntegral outLen / fromIntegral readCount
        putStrLn $ printf "Compressed block 0x%X - 0x%X (0x%X bytes)\n"
          gfxOffset (gfxOffset + readCount - 1) readCount
        putStrLn $ printf "Decompressed size 0x%X, compression raito %g\n"
          outLen compressRatio
        Bs.writeFile "decoded.bin" $ Bs.pack out
