module Kit where

primFirstRootRecord :: Int -> Int
primFirstRootRecord = primitive #firstRootRecord

firstRootRecord :: IO Int
firstRootRecord = return $ primFirstRootRecord 0

primRootRecordGetTopRecord :: Int -> Int
primRootRecordGetTopRecord = primitive #rootRecordGetTopRecord

rootRecordGetTopRecord :: Int -> IO Int
rootRecordGetTopRecord = return . primRootRecordGetTopRecord