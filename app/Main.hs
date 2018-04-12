module Main where

import FileParse
import System.IO
import System.Directory
import Data.List
import System.Process
import GHC.IO.Exception


main = do
    let fileName = "app/test-file.txt"
    let backupFile = (fileName ++ ".tut.backup")
    let kingTutOutput = ("king-tut-output.txt")
    let command = "F:/Developer/tools/Node/npm.cmd run test"

    -- Create handles for both the fileName and the backup file
    originalFileHandle <- openFile fileName ReadMode
    backupHandle <- openFile backupFile WriteMode

    -- Passes the contents of the file into contents
    contents <- hGetContents originalFileHandle

    -- Creates the backup file
    hPutStr backupHandle $ contents
    hClose backupHandle

    -- Testing parsing
    let (ParseAndTestInformationOutput (FileParsingInformation beforeStatement statement afterStatement) testCommand output) = (parseAndTestFile $ ParseAndTestInformationOutput (FileParsingInformation "" "" contents) command (return ("")))
    putStrLn(beforeStatement)
--     hPutStrLn output
    -- Create the new file with the handler
    kingTutOutputHandle <- openFile kingTutOutput AppendMode
    (parseToFileOutput output fileName) >>= hPutStr kingTutOutputHandle
    hClose kingTutOutputHandle

    -- Change the original file to remove the statement
    --removeFile fileName
    --renameFile tempFile fileName

    -- Run the tests

    -- Revert the file back to it's original state and delete other files
    removeFile fileName
    renameFile backupFile fileName
    putStrLn ("Finished!")