module Main where

import FileParse
import ConfigFile
import System.IO
import System.Directory
import System.Exit

main :: IO ()
main = do
    config <- readConfig "./king-tut.conf"
    putStrLn $ "The path value is: " ++ (srcFilePath config)
    putStrLn $ "The filename value is: " ++ (testingFileName config)
    let fileNameToTest = (srcFilePath config) ++ "/" ++ (testingFileName config)
    let backupFile = (fileNameToTest ++ ".tut.backup")
    let command = (inputTestCommand config)

    -- Tests the command above to ensure that it passes normally
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

    contents <- hGetContents originalFileHandle

    -- Creates the backup file
    hPutStr backupHandle $ contents
    hClose backupHandle

    -- Testing parsing
    (ParseAndTestInformationOutput (FileParsingInformation _ _ _) _ output _) <- (kingTut contents command fileNameToTest)

    -- Modify the output file for King Tut so that users can see how their application fared
    appendedFileContents <- (parseTestCommandOutputFile output fileNameToTest)
    _ <- writeToKingTutOutputFile appendedFileContents

    -- Revert the file back to it's original state and delete other files
    removeFile fileNameToTest
    renameFile backupFile fileNameToTest


    putStrLn ("Finished!")

