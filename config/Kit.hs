module Kit where

class DatabaseRecord a where
  get :: a -> String -> IO String
  set :: a -> String -> String -> IO ()

class Search a where
  hasElement :: a -> Bool
  next :: a -> IO a

newtype Record = Record Int

primTableCount :: Int
primTableCount = primitive #tableCount

primTableName :: Int -> String
primTableName = primitive #tableName

primGetRecord :: Int -> Int -> Int
primGetRecord = primitive #getRecord

primCloseRecord :: Int -> ()
primCloseRecord = primitive #closeRecord

primGetFieldCount :: Int -> Int
primGetFieldCount = primitive #getFieldCount

primGetFieldName :: Int -> Int -> String
primGetFieldName = primitive #getFieldName

primGetFieldValue :: Int -> String -> String
primGetFieldValue = primitive #getFieldValue

primExportXML :: String -> Int
primExportXML = primitive #exportxml

tableCount :: Int
tableCount = primTableCount

tableName :: Int -> String
tableName = primTableName

getRecord :: Int -> Int -> IO Int
getRecord x y = return $ primGetRecord x y

closeRecord :: Int -> IO Int
closeRecord = return . primCloseRecord

getFieldCount :: Int -> IO Int
getFieldCount r = return $ primGetFieldCount r

getFieldName :: Int -> Int -> IO String
getFieldName r f = return $ primGetFieldName r f

getFieldValue :: Int -> String -> IO String
getFieldValue r f = return $ primGetFieldValue r f

tables = map (\x->(tableName x, x)) [1 .. tableCount]

listTables = mapM_ putStrLn $ map (\ (x,y) -> show y ++ " " ++ x) tables

tableIndex n = go tables
   where go x = if null x then 0
                else if n == fst (head x)
                     then snd (head x)
                     else go (tail x)

   -- where go [] = 0
   --       go (x:xs) = if n == fst x then snd x else go xs

recNameValue r n = do
  fieldName <- getFieldName r n
  fieldValue <- getFieldValue r fieldName
  return (fieldName, fieldValue)


--  getFieldName r n >>= \ fn ->
--                   getFieldValue r fn >>= \ v ->
--                   return (fn, v)

--  reportRecord r = getFieldCount r >>= \n -> (mapM_ (\x -> recNameValue 1 x >>= \ (a,b) -> putStrLn (a ++ ": " ++ b)) [1 .. n])

reportRecord r = do
  n <- getFieldCount r
  mapM_ (\x -> recNameValue r x >>= \ (a,b) -> putStrLn (a ++ ": " ++ b)) [1 .. n]

  -- let putnv x = do (a,b) <- recNameValue r x
  --                  putStrLn $ a ++ ": " ++ b
  -- mapM_ putnv [1 .. n]

withRecord tableId recordId action = do
  rec <- getRecord tableId recordId
  action rec
  closeRecord rec
  return ()

exportXML :: String -> IO ()
exportXML path =
  return (primExportXML path) >>= print

