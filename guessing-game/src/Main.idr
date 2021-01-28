module Main

import Data.String

getInteger : IO Integer
getInteger = do
  putStr "guess a number: "
  n <- getLine
  case parsePositive n of
    Just n => pure n
    Nothing => do
      putStrLn (n ++ " is not a number")
      getInteger

getRandom : IO (Either FileError Int)
getRandom = do
  efile <- openFile "/dev/urandom" Read
  case efile of
    Left e => pure (Left e)
    Right file => do
      echars <- fGetChars file 4
      case echars of
        Left e => pure (Left e)
        Right chars => fGetInt file
 where
   stringToInt : String -> Int
   stringToInt chars = foldl (\acc, int => (acc `shiftL` 8) + int) 0 $ map ord $ unpack chars

   fGetInt : File -> IO (Either FileError Int)
   fGetInt file = do
     echars <- fGetChars file 4
     case echars of
       Left e => pure (Left e)
       Right chars => pure (Right (stringToInt chars))

game : Integer -> IO ()
game secret = do
  n <- getInteger
  case compare n secret of
    LT => do
      putStrLn "Too small"
      game secret
    EQ => putStrLn "You got it"
    GT => do
      putStrLn "Too big"
      game secret

main: IO ()
main = do
  Right int <- getRandom
    | pure ()
  game $ cast $ 1 + (int `mod` 100)
