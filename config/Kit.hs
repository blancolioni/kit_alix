module Kit where

class DatabaseRecord a where
  get :: a -> String -> IO String
  set :: a -> String -> String -> IO ()

class Search a where
  hasElement :: a -> Bool
  next :: a -> IO a

primGetRecord :: Int -> Int -> Int
primGetRecord = primitive #getRecord

primCloseRecord :: Int -> ()
primCloseRecord = primitive #closeRecord

primGetField :: Int -> String -> String
primGetField = primitive #getField

getRecord :: Int -> Int -> IO Int
getRecord x y = return $ primGetRecord x y

closeRecord :: Int -> IO Int
closeRecord = return . primCloseRecord

getField :: Int -> String -> IO String
getField r f = return $ primGetField r f
