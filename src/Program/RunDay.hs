{-# LANGUAGE TypeApplications #-}

module Program.RunDay (runDay, runDayWithIO, runDayWithParser, runDayWithParserAndIO, attoparsecParser, megaparsecParser, Day, Verbosity (Quiet, Timings, Verbose)) where

import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Functor
import Data.Text (pack)
import Data.Time (diffUTCTime, getCurrentTime)
import Program.Color
import System.Console.ANSI
import System.Directory (doesFileExist)
import Text.Printf
import qualified Text.Megaparsec as MP

data Verbosity = Quiet | Timings | Verbose deriving (Eq, Show, Ord)

type Day = Verbosity -> String -> IO (Maybe Double, Maybe Double)

attoparsecParser :: Parser a -> String -> Either String a
attoparsecParser inputParser fileContents = parseOnly inputParser . pack $ fileContents

megaparsecParser :: MP.ShowErrorComponent e => MP.Parsec e String a -> String -> Either String a
megaparsecParser inputParser fileContents = case MP.parse inputParser "" fileContents of
  Left bundle -> Left (MP.errorBundlePretty bundle)
  Right x -> Right x

runDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Program.RunDay.Day
runDay inputParser partA partB verbosity inputFile = runDayWithParserAndIO (attoparsecParser inputParser) (print . partA) (print . partB) verbosity inputFile

runDayWithParser :: (Show a, Show b, Show i) => (String -> Either String i) -> (i -> a) -> (i -> b) -> Program.RunDay.Day
runDayWithParser inputParser partA partB verbosity inputFile = runDayWithParserAndIO inputParser (print . partA) (print . partB) verbosity inputFile

runDayWithIO :: (Show i) => Parser i -> (i -> IO ()) -> (i -> IO ()) -> Program.RunDay.Day
runDayWithIO inputParser partA partB verbosity inputFile = runDayWithParserAndIO (attoparsecParser inputParser) partA partB verbosity inputFile

runDayWithParserAndIO :: (Show i) => (String -> Either String i) -> (i -> IO ()) -> (i -> IO ()) -> Program.RunDay.Day
runDayWithParserAndIO inputParser partA partB verbosity inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ readFile inputFile
        else
          throwError $
            unwords
              [ "I couldn't read the input!",
                "I was expecting it to be at",
                inputFile
              ]
    case (inputParser fileContents) of
      Left e -> throwError $ "Parser failed to read input. Error:\n" ++ e
      Right i -> do
        when (verbosity == Verbose) $ do
          liftIO $ putStrLn "Parser output:"
          liftIO $ print i
        return i

  case input of
    Left x -> withColor Red (putStrLn x) >> return (Nothing, Nothing)
    Right i -> do
      withColor Blue $ putStrLn "Part A:"
      time1 <- getCurrentTime
      successA <- catch ((partA i) $> True) $
        \(m :: SomeException) -> withColor Red $ do
          putStrLn "Couldn't run Part A!"
          when (verbosity == Verbose) $ print m
          return False
      time2 <- getCurrentTime

      let timeA = realToFrac $ diffUTCTime time2 time1
      when (verbosity >= Timings && successA) $ putStrLn $ printf "(%.2f)" timeA

      withColor Blue $ putStrLn "Part B:"
      successB <- catch ((partB i) $> True) $
        \(m :: SomeException) -> withColor Red $ do
          putStrLn "Couldn't run Part B!"
          when (verbosity == Verbose) $ print m
          return False
      time3 <- getCurrentTime

      let timeB = realToFrac $ diffUTCTime time3 time2
      when (verbosity >= Timings && successB) $ putStrLn $ printf "(%.2f)" timeB

      return $
        (,)
          (if successA then Just timeA else Nothing)
          (if successB then Just timeB else Nothing)
