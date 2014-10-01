import Data.List
import Data.Word
import Data.Map
import Data.Char
import Data.Maybe
import Data.Binary.Get
import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Binary.IEEE754

-------------------------------------------------
-- Basic
-------------------------------------------------

hasRepetitions :: (Eq a) => [a] -> Bool
hasRepetitions xs = nub xs /= xs

assert false msg _ = error ("Assertion failed: " ++ msg)
assert true  msg x = x

-------------------------------------------------
-- Model
-------------------------------------------------

type Id = Integer

class WithId el where
  getInGame :: Game -> Id -> el

data Name = Name String | Unnamed
            deriving Show

data Position = Pos { x :: Int, y :: Int } 
                deriving Show

data Group = Group { id :: Id, name :: Name }
             deriving Show

data Game = Game { groups :: [Group] }
            deriving Show

worldFileName = "worlds/seed_77.world"
worldBytes = S.readFile worldFileName

data PickleElement = PickleClass { moduleName :: String, className :: String } 
                     | PickleEmptyTuple
                     | PickleEmptyDict
                     | PickleList [PickleElement]
                     | PickleInstantiation PickleElement PickleElement
                     | PickleMark
                     | PickleString String
                     | PickleInt Int
                     | PickleFloat Double
                     deriving (Show, Eq)

type PickleStack = [PickleElement]
type PickleMemo  = Map Int PickleElement
data PickleStatus = PickleStatus { stack :: PickleStack, memo :: PickleMemo }
                    deriving Show

-- http://fossies.org/dox/Python-3.4.1/pickletools_8py_source.html
picklePush :: PickleStatus -> PickleElement -> PickleStatus
picklePush status e = PickleStatus s' m
                      where PickleStatus s m = status
                            s' = e:s

picklePop :: PickleStatus -> (PickleElement, PickleStatus)
picklePop status   = (h, status')
                     where PickleStatus s m = status
                           h:s' = s
                           status' = PickleStatus s' m

pickleStackTop :: PickleStatus -> PickleElement
pickleStackTop status = h
                        where PickleStatus s m = status
                              h:_ = s

getMaybe :: Maybe a -> a
getMaybe (Just v) = v

pickleConcat :: PickleElement -> [PickleElement] -> PickleElement
pickleConcat (PickleList l) es    = PickleList (l++es)

pickleGetMemo :: PickleStatus -> Int -> PickleElement
pickleGetMemo status k = getMaybe maybeV
                         where PickleStatus s m = status
                               maybeV = Data.Map.lookup k m 

pickleSetMemo :: PickleStatus -> Int -> PickleElement -> PickleStatus
pickleSetMemo status k v = status'
                           where PickleStatus s m = status
                                 status' = PickleStatus s (Data.Map.insert k v m)

--wf :: STMutable a File
--wf = File.new "worlds/seed_77.world"

readNlString' :: String -> S.ByteString -> IO (String, S.ByteString)
readNlString' s bytes = do let pn = S.head bytes
                           let bytes' = S.tail bytes
                           putStrLn $ "  reading " ++ (show (BS.w2c pn))
                           if pn == 10 then return (s,bytes') else readNlString' (s ++ [BS.w2c pn]) bytes'

readNlString :: S.ByteString -> IO (String, S.ByteString)
readNlString bytes = readNlString' "" bytes

readShortBinString' :: S.ByteString -> Int -> String -> (String, S.ByteString)
readShortBinString' bytes n s = if n==0 then (s,bytes) else let c = S.head bytes
                                                                bytes' = S.tail bytes
                                                                s' = s ++ [BS.w2c c]
                                                       in readShortBinString' bytes' (pred n) s'

readShortBinString :: S.ByteString -> (String, S.ByteString)
readShortBinString bytes = readShortBinString' bytes' (fromIntegral len) ""
                           where len = S.head bytes
                                 bytes' = S.tail bytes

picklePopUntilMark' :: PickleStatus -> [PickleElement] -> ([PickleElement],PickleStatus)
picklePopUntilMark' status es = if e==PickleMark then (es,status') else picklePopUntilMark' status' (e:es)
                                where (e,status') = picklePop status

picklePopUntilMark :: PickleStatus -> ([PickleElement],PickleStatus)
picklePopUntilMark status = picklePopUntilMark' status []

readWord64 :: S.ByteString -> (Word64, S.ByteString)
readWord64 bs = let val = runGet getWord64be bs
                    bs' = S.tail $ S.tail $ S.tail $ S.tail $ S.tail $ S.tail $ S.tail $ S.tail bs
                in (val,bs')

process :: PickleStatus -> S.ByteString -> IO PickleStatus
process status bytestring = do --putStrLn ("Processing " ++ (show l) ++ " bytes")
                              --putStrLn ("Status: "    ++ (show status))
                              let h = S.head bytestring
                              let bytestring' = S.tail bytestring
                              case h of
                                  128  -> do let pn = S.head bytestring'
                                             let bytestring'' = S.tail bytestring'
                                             putStrLn $ "Protocol " ++ (show pn)
                                             process status bytestring''
                                  -- \x81 NEWOBJ
                                  129 -> do let (args,status') = picklePop status 
                                            let (cls,status'') = picklePop status'
                                            let newobj = PickleInstantiation cls args
                                            let status''' = picklePush status'' newobj
                                            --putStrLn $ "NEWOBJ"
                                            process status''' bytestring'
                                  99  -> do (moduleName, bytestring'')  <- readNlString bytestring'                                                                                
                                            (className,  bytestring''') <- readNlString bytestring''
                                            putStrLn $ "GLOBAL " ++ moduleName ++ "." ++ className
                                            let status' = picklePush status (PickleClass moduleName className)
                                            process status' bytestring'''
                                  -- 'q' BINPUT
                                  113 ->  do let index = S.head bytestring'
                                             let bytestring'' = S.tail bytestring'
                                             let index' = fromIntegral index
                                             let e = pickleStackTop status
                                             let status' = pickleSetMemo status index' e
                                             process status' bytestring''
                                  -- 'r' LONG_BINPUT
                                  114 -> do let b0 = S.head bytestring
                                            let b1 = S.head $ S.tail bytestring 
                                            let b2 = S.head $ S.tail $ S.tail bytestring
                                            let b3 = S.head $ S.tail $ S.tail $ S.tail bytestring
                                            let bytestring'' = S.tail $ S.tail $ S.tail $ S.tail bytestring'
                                            let index = (fromIntegral b0) + ((fromIntegral b1)*256) + ((fromIntegral b2)*256*256) + ((fromIntegral b3)*256*256*256)
                                            let e = pickleStackTop status 
                                            let status' = pickleSetMemo status index e
                                            process status' bytestring''
                                  -- 'h' BINGET
                                  104 ->  do let index = S.head bytestring'
                                             let bytestring'' = S.tail bytestring'
                                             let e = pickleGetMemo status (fromIntegral index)
                                             let status' = picklePush status e
                                             process status' bytestring''
                                  -- 'G' BINFLOAT
                                  71  ->  do let (word64,bytestring'') = readWord64 bytestring'
                                             let val = wordToDouble word64
                                             let status' = picklePush status (PickleFloat val)
                                             process status' bytestring''                                         
                                  -- 'K' BININT1
                                  75  ->  do let val = S.head bytestring'
                                             let bytestring'' = S.tail bytestring'
                                             let status' = picklePush status (PickleInt (fromIntegral val))
                                             process status' bytestring''
                                  -- '(' MARK
                                  40 ->   do let status' = picklePush status PickleMark
                                             process status' bytestring'
                                  -- 'e' APPENDS
                                  -- pop elements until it finds a mark
                                  -- then pop the element before the mark (it should be a list)
                                  -- add everything to that list
                                  101 ->  do let (es,status') = picklePopUntilMark status
                                             let (l,status'') = picklePop status'
                                             let l' = l
                                             let l'' = pickleConcat l' es
                                             let status''' = picklePush status'' l''
                                             process status''' bytestring'
                                  -- 'U' SHORT_BINSTRING
                                  85 ->  do let (str,bytestring'') = readShortBinString bytestring'
                                            let status' = picklePush status (PickleString str)
                                            process status' bytestring''
                                  -- ')' EMPTY_TUPLE
                                  41 ->  do let status' = picklePush status PickleEmptyTuple
                                            process status' bytestring'
                                  -- ']' EMPTY_LIST
                                  93 ->  do let status' = picklePush status (PickleList [])
                                            process status' bytestring'                              
                                  -- '}' EMPTY_DICT
                                  125 ->  do let status' = picklePush status PickleEmptyDict
                                             process status' bytestring'             
                                  -- \x86 TUPLE2
                                  134 ->  do                                                                                                             
                                  otherwise -> do putStrLn $ "unknown " ++ (show h)
                                                  return status
                             

main :: IO ()
main = do putStrLn "Start"
          byteString <- S.readFile worldFileName :: IO S.ByteString
          --let res = unpickle byteString
          --let bytes = S.unpack byteString :: [Word8]      
          process (PickleStatus [] empty) byteString
          --case res of
          --     Left err -> putStrLn $ "Can't unpickle .\nUnpickling error:\n " ++ err
          --     Right v -> putStrLn "Well done!"
          putStrLn "Done"