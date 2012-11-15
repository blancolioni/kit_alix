module Kit where

primGetTable :: String -> Int
primGetTable = primitive #getTable

primTableName :: Int -> String
primTableName = primitive #tableName

primGetBy :: Int -> String -> String -> Int
primGetBy = primitive #getBy

primSelectBy :: Int -> String -> String -> [Int]
primSelectBy = primitive #selectBy

primTraceIndices :: Int -> String -> String -> Int
primTraceIndices = primitive #traceIndices

primGetField :: Int -> Int -> String -> String
primGetField = primitive #getField

primGetFloatField :: Int -> Int -> String -> Float
primGetFloatField = primitive #getFloatField

primGetIntField :: Int -> Int -> String -> Int
primGetIntField = primitive #getIntField

getTable :: String -> IO Int
getTable name = return $ primGetTable name

tableName :: Int -> IO String
tableName t = return $ primTableName t

getBy :: Int -> String -> String -> IO Int
getBy tableIndex keyName keyValue =
  return $ primGetBy tableIndex keyName keyValue

selectBy :: Int -> String -> String -> IO ([Int])
selectBy tableIndex keyName keyValue =
  return $ primSelectBy tableIndex keyName keyValue

getField :: Int -> Int -> String -> IO String
getField tableIndex recordIndex fieldName =
  return $ primGetField tableIndex recordIndex fieldName

getFloatField :: Int -> Int -> String -> IO Float
getFloatField tableIndex recordIndex fieldName =
  return $ primGetFloatField tableIndex recordIndex fieldName

getIntField :: Int -> Int -> String -> IO Int
getIntField tableIndex recordIndex fieldName =
  return $ primGetIntField tableIndex recordIndex fieldName

traceIndices :: Int -> String -> String -> IO Int
traceIndices tableIndex keyName keyValue =
  return $ primTraceIndices tableIndex keyName keyValue
