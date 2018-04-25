module Main where

import FileParse
import System.IO
import System.Directory
import System.Exit

main :: IO ()
main = do
    let fileNameToTest = "calculator/ts/src/add.ts"
    let backupFile = (fileNameToTest ++ ".tut.backup")
    let command = "F:/Developer/tools/Node/npm.cmd run test"

    doAllTestsPass <- executeSuccessful command
    if doAllTestsPass
       then putStrLn ("The command `" ++ command ++ "` can be executed")
       else do putStrLn "Commands all fail"
               let errorMessage = "\n\nThe command '" ++ command ++ "' failed to run. Please fix your tests or the command and try and run it again."
               _ <- writeToKingTutOutputFile errorMessage
               exitWith $ ExitFailure 1

    -- Create handles for both the fileNameToTest and the backup file
    originalFileHandle <- openFile fileNameToTest ReadMode
    backupHandle <- openFile backupFile WriteMode

    -- Passes the contents of the file into contents
    contents <- hGetContents originalFileHandle

    -- Creates the backup file
    hPutStr backupHandle $ contents
    hClose backupHandle

    -- Testing parsing
    (ParseAndTestInformationOutput (FileParsingInformation _ _ _) _ output _) <- (kingTut contents command fileNameToTest)
--     hPutStrLn output
    -- Create the new file with the handler
    appendedFileContents <- (parseToFileOutput output fileNameToTest)
    _ <- writeToKingTutOutputFile appendedFileContents
    -- Change the original file to remove the statement
    --removeFile fileNameToTest
    --renameFile tempFile fileNameToTest

    -- Run the tests

    -- Revert the file back to it's original state and delete other files
    removeFile fileNameToTest
    renameFile backupFile fileNameToTest


    putStrLn ("Finished!")

