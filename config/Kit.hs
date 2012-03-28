module Kit where

class DatabaseRecord a where
  get :: a -> String -> IO String
  set :: a -> String -> String -> IO ()

class Search a where
  hasElement :: a -> Bool
  next :: a -> IO a

primTableCount :: Int
primTableCount = primitive #tableCount

primTableName :: Int -> String
primTableName = primitive #tableName

primGetRecord :: Int -> Int -> Int
primGetRecord = primitive #getRecord

primCloseRecord :: Int -> ()
primCloseRecord = primitive #closeRecord

primGetField :: Int -> String -> String
primGetField = primitive #getField

tableCount :: Int
tableCount = primTableCount

tableName :: Int -> String
tableName = primTableName

getRecord :: Int -> Int -> IO Int
getRecord x y = return $ primGetRecord x y

closeRecord :: Int -> IO Int
closeRecord = return . primCloseRecord

getField :: Int -> String -> IO String
getField r f = return $ primGetField r f
