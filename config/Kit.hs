module Kit where

class DatabaseRecord a where
  get :: a -> String -> IO String
  set :: a -> String -> String -> IO ()

class Search a where
  hasElement :: a -> Bool
  next :: a -> IO a

primFirstRootRecord :: Int -> Int
primFirstRootRecord = primitive #firstRootRecord

firstRootRecord :: IO Int
firstRootRecord = return $ primFirstRootRecord 0

primRootRecordGetTopRecord :: Int -> Int
primRootRecordGetTopRecord = primitive #rootRecordGetTopRecord

rootRecordGetTopRecord :: Int -> IO Int
rootRecordGetTopRecord = return . primRootRecordGetTopRecord

primNamedItemGetName :: Int -> String
primNamedItemGetName = primitive #namedItemGetName

namedItemGetName :: Int -> IO String
namedItemGetName = return . primNamedItemGetName

primReportRecord :: Int -> Int -> Int
primReportRecord = primitive #reportRecord

reportRecord :: Int -> Int -> IO Int
reportRecord table index = return $ primReportRecord table index
