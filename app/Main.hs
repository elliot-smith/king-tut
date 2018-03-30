import System.IO
import System.Directory
import Data.List

main = do
    let fileName = "test-file.txt"
    let currDir = withCurrentDirectory
    let backupFile = (fileName ++ ".tut.backup")
    let tempFile = (fileName ++ ".tut.tmp")

    -- Create handles for both the fileName and the backup file
    originalFileHandle <- openFile fileName ReadMode
    backupHandle <- openFile backupFile WriteMode

    -- Passes the contents of the file into contents
    contents <- hGetContents originalFileHandle

    -- Creates the backup file
    hPutStr backupHandle $ contents
    hClose backupHandle

    -- Testing parsing
    putStrLn ("next test" ++ parseFile "" "" contents)

    -- Create the new file with the handler
    tempFileHandle <- openFile tempFile WriteMode
    hClose tempFileHandle

    -- Change the original file to remove the statement
    removeFile fileName
    renameFile tempFile fileName

    -- Run the tests

    -- Revert the file back to it's original state and delete other files
    removeFile fileName
    renameFile backupFile fileName

goThroughFile :: String -> IO ()
goThroughFile originalString = do
    putStrLn originalString

parseAndTestFile :: String -> String
parseAndTestFile contents = do
    parseFile "" "" contents

--End of file
parseFile :: String -> String -> String -> String
parseFile before current "" = before

--Parse File
parseFile before current after = do
    if checkEndOfStatement (after !! 0)
    then (parseFile (before ++ [(after !! 0)]) "" "")
    else (parseFile (before ++ [(after !! 0)]) "" (tail after))

checkEndOfStatement :: Char -> Bool
checkEndOfStatement character = do
    if character == ';'
    then True
    else False
