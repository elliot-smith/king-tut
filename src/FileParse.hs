module FileParse
    (parseAndTestFile,
    getNextStatement,
    checkEndOfStatement,
    executeAndReturnOutput,
    FileParsingInformation  (..),
    ParseAndTestInformationOutput (..),
    splitStringOnDelimeter,
    (+++),
    parseToFileOutput,
    executeSuccessful,
    writeToKingTutOutputFile
    ) where

import Data.Char
import System.IO
import System.Directory
import Data.List
import System.Process
import GHC.IO.Exception

data FileParsingInformation = FileParsingInformation{ beforeStatement :: String,
                                                      statement :: String,
                                                      afterStatement :: String} deriving (Eq, Show)

-- instance Eq FileParsingInformation where
--   (FileParsingInformation) (FileParsingInformation before)

data ParseAndTestInformationOutput = ParseAndTestInformationOutput { allStatements :: FileParsingInformation,
                                                         testCommand :: String,
                                                         commandOutput :: IO String,
                                                         fileName :: String }

-- Credits to Chris Taylor from https://stackoverflow.com/questions/20645805/haskell-concat-two-io-strings
(+++) :: Monad m => m [a] -> m [a] -> m [a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

-- Second onwards passing of the file contents
-- Receives statement "" contentOfFile-statement, testCommand, CommandOutput
-- Output statement "" contentOfFile-statement, testCommand, CommandOutput
-- Finishes when the file is completely parsed
parseAndTestFile :: ParseAndTestInformationOutput -> ParseAndTestInformationOutput
parseAndTestFile (ParseAndTestInformationOutput (FileParsingInformation beforeStatement statement afterStatement) testCommand allCommandOutputs fileName) =
    do let FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement = (getNextStatement beforeStatement statement afterStatement)
       let isFileOverwritten = writeToOriginalFile (nextBeforeStatement ++ nextAfterStatement) fileName
       case isFileOverwritten of
        returnIOBool -> testFile (ParseAndTestInformationOutput (FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement) testCommand allCommandOutputs fileName)
        _ -> (ParseAndTestInformationOutput (FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement) testCommand allCommandOutputs fileName)

testFile :: ParseAndTestInformationOutput -> ParseAndTestInformationOutput
testFile (ParseAndTestInformationOutput (FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement) testCommand allCommandOutputs fileName) = do
    let output = allCommandOutputs +++ (executeAndReturnOutput testCommand nextStatement)

    case length nextAfterStatement of
      0 -> (ParseAndTestInformationOutput (FileParsingInformation (nextBeforeStatement ++ nextStatement) "" nextAfterStatement) testCommand output fileName)
      _ -> parseAndTestFile (ParseAndTestInformationOutput (FileParsingInformation (nextBeforeStatement ++ nextStatement) "" nextAfterStatement) testCommand output fileName)

returnIOBool :: IO Bool
returnIOBool = return True

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

executeAndReturnOutput :: String -> String -> IO String
executeAndReturnOutput cmd deletedStatement = do
    isExecuteSuccessful <- executeSuccessful cmd
    case isExecuteSuccessful of
     True -> return ("Deleting the statement " ++ deletedStatement ++ " did not fail any tests. Please look into this!\n\n")
     _           -> return ("")

executeSuccessful :: String -> IO Bool
executeSuccessful cmd = do
    let splitCommand = splitStringOnDelimeter cmd ' '
    (exitCode, output, errOutput) <- readProcessWithExitCode (head splitCommand) (tail splitCommand) ""
    case exitCode of
     ExitSuccess -> return (True)
     _           -> return (False)

splitStringOnDelimeter :: String -> Char -> [String]
splitStringOnDelimeter "" delimeter = [""]

-- Credits to Willem Van Onsem from https://stackoverflow.com/a/49611655/6063754
splitStringOnDelimeter (h:t) delimeter | h == delimeter = "" : split
                                       | otherwise = (h : sh) : st
    where split@(sh:st) = splitStringOnDelimeter t delimeter

-- Parse file output to notify runner that it was successful
parseToFileOutput :: IO String -> String -> IO String
parseToFileOutput parseOutputIO filename = do
   parseOutput <- parseOutputIO
   if parseOutput == ""
     then return ("\n\nThe file " ++ filename ++ " successfully failed when lines where deleted! Well done!")
     else return ("\n\n" ++ parseOutput)

writeToKingTutOutputFile :: IO String -> IO Bool
writeToKingTutOutputFile outputText = do
    let kingTutOutputFile = ("king-tut-output.txt")
    kingTutOutputHandle <- openFile kingTutOutputFile AppendMode
    outputText >>= hPutStr kingTutOutputHandle
    hClose kingTutOutputHandle
    return True

writeToOriginalFile :: String -> String -> IO Bool
writeToOriginalFile newFileContents fileName = do
    kingTutOutputHandle <- openFile fileName WriteMode
    hPutStr kingTutOutputHandle newFileContents
    hClose kingTutOutputHandle
    kingTutOutputHandle <- openFile "hello" WriteMode
    hPutStr kingTutOutputHandle newFileContents
    hClose kingTutOutputHandle
    return True