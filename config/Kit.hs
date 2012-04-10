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

tables = map (\x->(tableName x, x)) [1 .. tableCount]

listTables = mapM_ putStrLn $ map (\ (x,y) -> show y ++ " " ++ x) tables

tableIndex n = go tables
   where go x = if null x then 0
                else if n == fst (head x)
                     then snd (head x)
                     else go (tail x)

   -- where go [] = 0
   --       go (x:xs) = if n == fst x then snd x else go xs

test_1 [] = False
test_1 (x:xs) = x == 1 || test_1 xs

