module FileParse
    (parseAndTestFileFirst,
    parseAndTestFile,
    getNextStatement,
    checkEndOfStatement,
    exec,
    FileParsingInformation  (..),
    ParseAndTestInformation  (..),
    ParseAndTestInformationOutput (..)
    ) where

import Data.Char
import System.IO
import System.Directory
import Data.List
import System.Process
import GHC.IO.Exception

data FileParsingInformation = FileParsingInformation{ beforeStatement :: String,
                                                      statement :: String,
                                                      afterStatement :: String } deriving (Eq, Show)

-- instance Eq FileParsingInformation where
--   (FileParsingInformation) (FileParsingInformation before)

data ParseAndTestInformation = ParseAndTestInformation { statements :: FileParsingInformation,
                                                         testCommand :: String } deriving (Eq, Show)

data ParseAndTestInformationOutput = ParseAndTestInformationOutput { statementsOutput :: FileParsingInformation,
                                                         testCommandSecond :: String ,
                                                         commandOutput :: IO String }

-- Credits to Chris Taylor from https://stackoverflow.com/questions/20645805/haskell-concat-two-io-strings
(+++) :: Monad m => m [a] -> m [a] -> m [a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

-- First parse of the file contents
-- Receives "" "" contentOfFile, testCommand
-- Output statement "" contentOfFile-statement, testCommand, CommandOutput
parseAndTestFileFirst :: ParseAndTestInformation -> ParseAndTestInformationOutput
parseAndTestFileFirst (ParseAndTestInformation (FileParsingInformation beforeStatement statement afterStatement) testCommand) = do
    let (FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement) = (getNextStatement beforeStatement statement afterStatement)

    -- Run Tests
    let commandOutput = exec testCommand nextStatement
    if length nextAfterStatement == 1
    then (ParseAndTestInformationOutput (FileParsingInformation (nextBeforeStatement ++ nextStatement) "" nextAfterStatement) testCommand commandOutput)
    else parseAndTestFile (ParseAndTestInformationOutput (FileParsingInformation (nextBeforeStatement ++ nextStatement) "" nextAfterStatement) testCommand commandOutput)

-- Second onwards passing of the file contents
-- Receives statement "" contentOfFile-statement, testCommand, CommandOutput
-- Output statement "" contentOfFile-statement, testCommand, CommandOutput
-- Finishes when the file is completely parsed
parseAndTestFile :: ParseAndTestInformationOutput -> ParseAndTestInformationOutput
parseAndTestFile (ParseAndTestInformationOutput (FileParsingInformation beforeStatement statement afterStatement) testCommandSecond allCommandOutputs) = do
    let FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement = (getNextStatement beforeStatement statement afterStatement)

    -- Run Tests
    let output = allCommandOutputs +++ (exec testCommandSecond nextStatement)
    if length nextAfterStatement == 0
    then (ParseAndTestInformationOutput (FileParsingInformation (nextBeforeStatement ++ nextStatement) "" nextAfterStatement) testCommandSecond output)
    else parseAndTestFile (ParseAndTestInformationOutput (FileParsingInformation (nextBeforeStatement ++ nextStatement) "" nextAfterStatement) testCommandSecond output)

-- End of file
-- Checks for end of file
getNextStatement :: String -> String -> String -> FileParsingInformation
getNextStatement beforeStatement currentStatement "" = FileParsingInformation beforeStatement currentStatement ""

--Parse File
-- If the next statement passes checkEndOfStatement it then returns the previous beforeStatement, currentStatement plus head of afterStatement, tail afterStatement
-- If it doesn't then it calls itself again with beforeStatement, currentStatement plus head of afterStatement, tail afterStatement
getNextStatement beforeStatement currentStatement afterStatement =
    if checkEndOfStatement (afterStatement !! 0)
    then FileParsingInformation beforeStatement (currentStatement ++ [(afterStatement !! 0)]) (tail afterStatement)
    else (getNextStatement beforeStatement (currentStatement ++ [(afterStatement !! 0)]) (tail afterStatement))

checkEndOfStatement :: Char -> Bool
checkEndOfStatement character =
    if character == ';'
    then True
    else False

exec :: String -> String -> IO String
exec cmd deletedStatement = do
    let splitCommand = splitStringOnDelimeter cmd ' '
    (exitCode, output, errOutput) <- readProcessWithExitCode (head splitCommand) (tail splitCommand) ""
    if exitCode == ExitSuccess
    then return ("Deleting the statement " ++ deletedStatement ++ " did not fail any tests. Please look into this!\n\n")
    else return ("")

splitStringOnDelimeter :: String -> Char -> [String]
splitStringOnDelimeter "" delimeter = [""]

-- Credits to Willem Van Onsem from https://stackoverflow.com/a/49611655/6063754
splitStringOnDelimeter (h:t) delimeter | h == delimeter = "" : split
                                       | otherwise = (h : sh) : st
    where split@(sh:st) = splitStringOnDelimeter t delimeter
