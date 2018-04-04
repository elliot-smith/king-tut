module Main where

import FileParse
import System.IO
import System.Directory
import Data.List
import System.Process
import GHC.IO.Exception


main = do
    let fileName = "app/test-file.txt"
    let currDir = withCurrentDirectory
    let backupFile = (fileName ++ ".tut.backup")
    let tempFile = (fileName ++ ".tut.tmp")
    let command = "echo hello world"

    -- Create handles for both the fileName and the backup file
    originalFileHandle <- openFile fileName ReadMode
    backupHandle <- openFile backupFile WriteMode

    -- Passes the contents of the file into contents
    contents <- hGetContents originalFileHandle

    -- Creates the backup file
    hPutStr backupHandle $ contents
    hClose backupHandle

    -- Testing parsing
    let (ParseAndTestInformationOutput (FileParsingInformation beforeStatement statement afterStatement) testCommand output) = (parseAndTestFileFirst $ ParseAndTestInformation (FileParsingInformation "" "" contents) command)
    putStrLn(beforeStatement)
--     hPutStrLn output
    -- Create the new file with the handler
    tempFileHandle <- openFile tempFile WriteMode
    output >>= hPutStr tempFileHandle
    hClose tempFileHandle

    -- Change the original file to remove the statement
    --removeFile fileName
    --renameFile tempFile fileName

    -- Run the tests

    -- Revert the file back to it's original state and delete other files
    removeFile fileName
    renameFile backupFile fileName
    putStrLn ("Finished!")

