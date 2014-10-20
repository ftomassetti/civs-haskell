{-# LANGUAGE DeriveDataTypeable #-}

module Civs.Pickle where

import Data.Word
import Data.Map
import Data.Binary.Get
import Data.Typeable
import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Internal as BS (w2c)
import Data.Binary.IEEE754

data PickleElement = PickleClass { moduleName :: String, className :: String } 
                     | PickleSetState PickleElement PickleElement
                     | PickleEmptyTuple
                     | PickleTuple2 PickleElement PickleElement
                     | PickleDict (Map PickleElement PickleElement)
                     | PickleList [PickleElement]
                     | PickleInstantiation PickleElement PickleElement
                     | PickleMark
                     | PickleBool Bool
                     | PickleString String
                     | PickleInt Int
                     | PickleFloat Double                     
                     | PickleNone
                     deriving (Show, Eq, Ord, Typeable)

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

readNlString' :: String -> S.ByteString -> (String, S.ByteString)
readNlString' s bytes = let pn = S.head bytes
                            bytes' = S.tail bytes
                        in if pn == 10 then (s,bytes') else readNlString' (s ++ [BS.w2c pn]) bytes'

readNlString :: S.ByteString -> (String, S.ByteString)
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

processSetItemsReading :: PickleStatus -> Map PickleElement PickleElement -> (Map PickleElement PickleElement, PickleStatus)
processSetItemsReading st entries    = let (head,st') = picklePop st
                                       in case head of PickleMark -> (entries, st')
                                                       _          -> let (key,st'') = picklePop st'
                                                                         entries' = Data.Map.insert key head entries
                                                                     in processSetItemsReading st'' entries'  

-- Pop pairs of key-values from the stack until a Mark is found
-- Pop the dict below the mark
-- Push the modified dict     
processSetItems :: PickleStatus -> PickleStatus
processSetItems st  =   let (entries,st') = processSetItemsReading st (Data.Map.empty)
                            (dict,st'') = picklePop st'
                            dict' = case dict of PickleDict existingEntries -> PickleDict (Data.Map.union existingEntries entries)
                                                 _ -> error "Expecting a dict on the stack"
                            st''' = picklePush st'' dict'
                        in st'''

readUint16 :: S.ByteString -> (Int, S.ByteString)
readUint16 bs = let b0 = S.head bs
                    b1 = S.head $ S.tail bs                    
                    bs' = S.tail $ S.tail bs
                    index = (fromIntegral b0) + ((fromIntegral b1)*256)
                in (index, bs')

readUint64 :: S.ByteString -> (Int, S.ByteString)
readUint64 bs = let b0 = S.head bs
                    b1 = S.head $ S.tail bs
                    b2 = S.head $ S.tail $ S.tail bs
                    b3 = S.head $ S.tail $ S.tail $ S.tail bs
                    bs' = S.tail $ S.tail $ S.tail $ S.tail bs
                    index = (fromIntegral b0) + ((fromIntegral b1)*256) + ((fromIntegral b2)*256*256) + ((fromIntegral b3)*256*256*256)
                in (index, bs')

process :: PickleStatus -> S.ByteString -> IO PickleElement
process status bytestring = do --putStrLn ("Processing " ++ (show l) ++ " bytes")
                              --putStrLn ("Status: "    ++ (show status))
                              let h = S.head bytestring
                              let bytestring' = S.tail bytestring
                              case h of
                                  128  -> do let pn = S.head bytestring'
                                             let bytestring'' = S.tail bytestring'
                                             process status bytestring''
                                  -- \x81 NEWOBJ
                                  129 -> do let (args,status') = picklePop status 
                                            let (cls,status'') = picklePop status'
                                            let newobj = PickleInstantiation cls args
                                            let status''' = picklePush status'' newobj
                                            --putStrLn $ "NEWOBJ"
                                            process status''' bytestring'
                                  99  -> do let (moduleName, bytestring'')  = readNlString bytestring'                                                                                
                                            let (className,  bytestring''') = readNlString bytestring''
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
                                  -- Store the stack top into the memo. The stack is not popped.
                                  -- The index of the memo location to write into is given by the 4-byte
                                  -- unsigned little-endian integer following
                                  114 -> do let (index,bytestring'') = readUint64 bytestring'
                                            let e = pickleStackTop status 
                                            let status' = pickleSetMemo status index e
                                            process status' bytestring''
                                  -- 'h' BINGET
                                  104 ->  do let index = S.head bytestring'
                                             let bytestring'' = S.tail bytestring'
                                             let e = pickleGetMemo status (fromIntegral index)
                                             let status' = picklePush status e
                                             process status' bytestring''
                                  -- 'j' LONG_BINGET
                                  -- Read an object from the memo and push it on the stack.
                                  -- The index of the memo object to push is given by the 4-byte unsigned
                                  -- little-endian integer following
                                  106 ->  do let (index,bytestring'') = readUint64 bytestring'
                                             let el = pickleGetMemo status (fromIntegral index)
                                             let status' = picklePush status el
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
                                  -- 'M' BININT2
                                  77  ->  do let (val,bytestring'') = readUint16 bytestring'
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
                                  125 ->  do let status' = picklePush status (PickleDict Data.Map.empty)
                                             process status' bytestring'             
                                  -- \x86 TUPLE2
                                  -- Build a two-tuple out of the top two items on the stack
                                  -- Push the built tuple
                                  134 ->  do let (el2,status') = picklePop status 
                                             let (el1,status'') = picklePop status'
                                             let t = PickleTuple2 el1 el2
                                             let status''' = picklePush status'' t
                                             process status''' bytestring' 
                                  -- \x88 NEWTRUE
                                  136 ->  do let status' = picklePush status (PickleBool True)
                                             process status' bytestring' 
                                  -- \x89 NEWFALSE
                                  137 ->  do let status' = picklePush status (PickleBool False)
                                             process status' bytestring'                                              
                                  -- 'N' NONE
                                  78 ->   do let status' = picklePush status PickleNone
                                             process status' bytestring'  
                                  -- 'u' SETITEMS
                                  117 ->  do process (processSetItems status) bytestring'

                                  -- 'b' BUILD
                                  98  ->  do let (arg, status') = picklePop status 
                                             let (obj, status'') = picklePop status'
                                             let obj' = PickleSetState obj arg
                                             let status''' = picklePush status'' obj'
                                             process status''' bytestring'

                                  -- '.' STOP
                                  46  ->  do let (head, status') = picklePop status
                                             return head

                                  otherwise -> do putStrLn $ "unknown " ++ (show h)
                                                  let bytes = S.unpack bytestring :: [Word8]
                                                  let l = length bytes
                                                  putStrLn $ "Remaining bytes are " ++ (show l)
                                                  return PickleNone

printPickle :: PickleElement -> String
printPickle el = case el of
                      PickleClass mn cs -> "Class" ++ mn ++ "." ++ cs
                      PickleInstantiation a b -> "Instantiation of (" ++ (printPickle a) ++ ") args: (" ++ (printPickle b) ++ ")"
                      PickleList l -> "List"
                      PickleNone -> "None"
                      PickleEmptyTuple -> "'()"
                      PickleDict d -> "Dict "++(show $ keys d)
                      PickleSetState obj st -> "SetState (" ++ (printPickle obj) ++ ") state: (" ++ (printPickle st) ++ ")"
                      _          -> "...incomplete"

toString (PickleString s) = s
toDict :: PickleElement -> Map String PickleElement
toDict (PickleDict d)     = Data.Map.mapKeys toString d
toInt (PickleInt i)       = i
toDouble (PickleFloat f)     = f
toList (PickleList l)     = l
toList e = error $ "I canno convert to list "++(printPickle e)
toTuple2 (PickleTuple2 a b) = (a, b)