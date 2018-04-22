module Main where

import FileParse
import System.IO
import System.Directory
import Data.List
import System.Process
import System.Exit
import GHC.IO.Exception


main = do
    let fileName = "calculator/ts/src/add.ts"
    let backupFile = (fileName ++ ".tut.backup")
    let command = "F:/Developer/tools/Node/npm.cmd run test"

--     doAllTestsPass <- executeSuccessful command
--     if doAllTestsPass
--        then putStrLn "All commands pass"
--        else do putStrLn "Commands all fail"
--                writeToKingTutOutputFile $ return ("\n\nThe command '" ++ command ++ "' failed to run. Please fix your tests or the command and try and run it again.")
--                exitWith $ ExitFailure 1

    -- Create handles for both the fileName and the backup file
    originalFileHandle <- openFile fileName ReadMode
    backupHandle <- openFile backupFile WriteMode

    -- Passes the contents of the file into contents
    contents <- hGetContents originalFileHandle

    -- Creates the backup file
    hPutStr backupHandle $ contents
    hClose backupHandle

    -- Testing parsing
    let (ParseAndTestInformationOutput (FileParsingInformation beforeStatement statement afterStatement) testCommand output fileName) = (parseAndTestFile $ ParseAndTestInformationOutput (FileParsingInformation "" "" contents) command (return ("")) fileName )
    putStrLn(beforeStatement)
--     hPutStrLn output
    -- Create the new file with the handler
    writeToKingTutOutputFile (parseToFileOutput output fileName)
    -- Change the original file to remove the statement
    --removeFile fileName
    --renameFile tempFile fileName

    -- Run the tests

    -- Revert the file back to it's original state and delete other files
    removeFile fileName
    renameFile backupFile fileName


    putStrLn ("Finished!")

