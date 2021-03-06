module FileParse
    (kingTut,
    parseAndOverwriteOriginalFile,
    getNextStatement,
    checkEndOfStatement,
    executeAndReturnOutput,
    FileParsingInformation  (..),
    ParseAndTestInformationOutput (..),
    splitStringOnDelimeter,
    parseTestCommandOutputFile,
    executeSuccessful,
    writeToKingTutOutputFile
    ) where

import System.IO
import System.Process
import GHC.IO.Exception

data FileParsingInformation = FileParsingInformation{ beforeCurrentStatement :: String,
                                                      currentStatement :: String,
                                                      afterCurrentStatement :: String} deriving (Eq, Show)

data ParseAndTestInformationOutput = ParseAndTestInformationOutput { allStatements :: FileParsingInformation,
                                                         testCommand :: String,
                                                         commandOutput :: String,
                                                         fileName :: String } deriving (Eq, Show)

kingTut :: String -> String -> String -> IO ParseAndTestInformationOutput
kingTut fileContents kingTutTestCommand testingFileName = do
   parseAndOverwriteOriginalFile (return (ParseAndTestInformationOutput (FileParsingInformation "" "" fileContents) kingTutTestCommand "" testingFileName))

parseAndOverwriteOriginalFile :: IO ParseAndTestInformationOutput -> IO ParseAndTestInformationOutput
parseAndOverwriteOriginalFile parsedInformation = do
   (ParseAndTestInformationOutput (FileParsingInformation beforeStatement _ afterStatement) testFileCommand allCommandOutputs testFileName) <- parsedInformation
   let FileParsingInformation beforeNextStatement nextStatement afterNextStatement = (getNextStatement beforeStatement "" afterStatement)
   isFileOverwritten <- (writeToOriginalFile (beforeNextStatement ++ afterNextStatement) testFileName)

   let parseAndOverwriteOriginalFileReturnObject = return (ParseAndTestInformationOutput (FileParsingInformation beforeNextStatement nextStatement afterNextStatement) testFileCommand allCommandOutputs testFileName)
   case isFileOverwritten of
      True -> testFile parseAndOverwriteOriginalFileReturnObject
      False -> parseAndOverwriteOriginalFileReturnObject

testFile :: IO ParseAndTestInformationOutput -> IO ParseAndTestInformationOutput
testFile testFileInput = do
    (ParseAndTestInformationOutput (FileParsingInformation beforeTestStatement currentTestStatement afterTestStatement) commandToTestFile allCommandOutputs fileNameThatIsTested) <- testFileInput
    outputs <- (executeAndReturnOutput commandToTestFile currentTestStatement)
    let output = allCommandOutputs ++ outputs

    let testFileReturnObject = return (ParseAndTestInformationOutput (FileParsingInformation (beforeTestStatement ++ currentTestStatement) "" afterTestStatement) commandToTestFile output fileNameThatIsTested)
    case length afterTestStatement of
        0 -> testFileReturnObject
        _ -> parseAndOverwriteOriginalFile testFileReturnObject

getNextStatement :: String -> String -> String -> FileParsingInformation
getNextStatement beforeNextStatement nextStatement "" = FileParsingInformation beforeNextStatement nextStatement ""

getNextStatement beforeNextStatement nextStatement afterNextStatement =
    if checkEndOfStatement (afterNextStatement !! 0)
    then FileParsingInformation beforeNextStatement (nextStatement ++ [(afterNextStatement !! 0)]) (tail afterNextStatement)
    else (getNextStatement beforeNextStatement (nextStatement ++ [(afterNextStatement !! 0)]) (tail afterNextStatement))

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
       _    -> return ("")

executeSuccessful :: String -> IO Bool
executeSuccessful cmd = do
    let splitCommand = splitStringOnDelimeter cmd ' '
    (exitCode, _, _) <- readProcessWithExitCode (head splitCommand) (tail splitCommand) ""
    case exitCode of
     ExitSuccess -> return (True)
     _           -> return (False)

splitStringOnDelimeter :: String -> Char -> [String]
splitStringOnDelimeter "" _ = [""]

-- Credits to Willem Van Onsem from https://stackoverflow.com/a/49611655/6063754
splitStringOnDelimeter (h:t) delimeter | h == delimeter = "" : split
                                       | otherwise = (h : sh) : st
    where split@(sh:st) = splitStringOnDelimeter t delimeter

parseTestCommandOutputFile :: String -> String -> IO String
parseTestCommandOutputFile parseOutput filename = do
   if parseOutput == ""
        then do
            putStrLn "All tests failed. Congratulations!"
            let successMessage = "\n\nThe file " ++ filename ++ " successfully failed when lines where deleted! Well done!"
            return (successMessage)
        else do
            putStrLn "At least one test passed. Please check the file king-tut-output.txt for more information"
            return ("\n\n" ++ parseOutput)

writeToKingTutOutputFile :: String -> IO Bool
writeToKingTutOutputFile commandOutputText = do
    let kingTutOutputFile = ("king-tut-output.txt")
    kingTutCommandOutputHandle <- openFile kingTutOutputFile AppendMode
    hPutStr kingTutCommandOutputHandle commandOutputText
    hClose kingTutCommandOutputHandle
    return True

writeToOriginalFile :: String -> String -> IO Bool
writeToOriginalFile newFileContents originalFileName = do
    kingTutOriginalFileHandle <- openFile originalFileName WriteMode
    hPutStr kingTutOriginalFileHandle newFileContents
    hClose kingTutOriginalFileHandle
    return True