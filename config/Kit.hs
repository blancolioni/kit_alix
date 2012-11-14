module Kit where

primGetTable :: String -> Int
primGetTable = primitive #getTable

primTableName :: Int -> String
primTableName = primitive #tableName

primGetBy :: Int -> String -> String -> Int
primGetBy = primitive #getBy

primTraceIndices :: Int -> String -> String -> Int
primTraceIndices = primitive #traceIndices

primGetField :: Int -> Int -> String -> String
primGetField = primitive #getField

getTable :: String -> IO Int
getTable name = return $ primGetTable name

tableName :: Int -> IO String
tableName t = return $ primTableName t

getBy :: Int -> String -> String -> IO Int
getBy tableIndex keyName keyValue =
  return $ primGetBy tableIndex keyName keyValue

getField :: Int -> Int -> String -> IO String
getField tableIndex recordIndex fieldName =
  return $ primGetField tableIndex recordIndex fieldName

traceIndices :: Int -> String -> String -> IO Int
traceIndices tableIndex keyName keyValue =
  return $ primTraceIndices tableIndex keyName keyValue
