-- You can use this file to test your functions: `cabal run` executes main.
-- For example, if main is set to mainDateTime or mainCalendar:
-- echo "19970610T172345Z" | cabal run
-- cat examples/bastille.ics | cabal run
-- Feel free to use ghci instead, or to change functions here to test whatever you want.
-- We'll ignore anything in this file when grading!

-- Made by: Gijs Koppenberg (0779342) and Jason van Otterlo


module Main where

import DateTime
import Calendar
import Features
import System.Environment
import System.IO
import Text.Read (Lexeme(String))
import Control.Exception (handle)


data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = do
  setNewlineTranslations
  --mainDateTime
  --testCalendar
  testTokenCalendar
  --testFeature

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
  where
    processInput = map (run parseDateTime) . lines
    processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
    printOutput  = unlines . map show

testDateTime :: IO ()
testDateTime = interact (show . res . test)
  where test s = run parseDateTime s 
        res dt = case dt of
                   Just dt -> printDateTime dt
                   Nothing -> "Error: couldn't parse string"

testCalendar :: IO ()
testCalendar = do
        withFile "examples/rooster_infotc.ics" ReadMode $ \handle -> do
          read <- hGetContents handle
          putStr (getres (run pCal read))
        
        return ()

        where getres c = case c of
                    Just c -> printCalendar c
                    Nothing -> "Error: couldn't parse string"

testTokenCalendar :: IO ()
testTokenCalendar = do
        withFile "examples/rooster_infotc.ics" ReadMode $ \handle -> do
          read <- hGetContents handle
          let intermediate = getinter (run lexCalendar read)
          putStr (getres (run parseCalendar intermediate))
        
        return ()

        where getres c = case c of
                    Just c -> printCalendar c
                    Nothing -> "Error"
              getinter ts = case ts of 
                    Just ts -> ts
                    Nothing -> []

testFeature :: IO ()
testFeature = do
        withFile "examples/multiline.ics" ReadMode $ \handle -> do
          read <- hGetContents handle
          putStr (show (timeSpent "This is a very long description th at spans over multiple lines." (getres (run pCal read))))
        
        return ()

        where getres c = case c of
                    Just c -> c
                    Nothing -> Calendar [] []



mainCalendar :: IO ()
mainCalendar = interact (show . recognizeCalendar)

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  string <- readFileWindows path
  return $ recognizeCalendar string

-- These three functions fight Windows newline translations:
-- without them, on Windows machines, "\r\n" will be read as "\n"
-- and "\n" will be written as "\r\n".
-- Test using these functions rather than "readFile", and parse  newlines as "\r\n",
-- to make sure your parser works on all operating systems (i.e. also for your grader)!
setNewlineTranslations :: IO ()
setNewlineTranslations = do
  hSetNewlineMode stdin  noNewlineTranslation
  hSetNewlineMode stdout noNewlineTranslation
readFileWindows :: FilePath -> IO String
readFileWindows p = withBinaryFile p ReadMode hGetContents
writeFileWindows :: FilePath -> String -> IO ()
writeFileWindows p s = withBinaryFile p WriteMode (`hPutStr` s)
