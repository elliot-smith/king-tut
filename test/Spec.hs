import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import FileParse

-- fakeFile :: String -> IO String
-- fakeFile fileName = return "Fake content"

main :: IO ()
main = hspec $ do
  describe "FileParse" $ do
      describe "checkEndOfStatement" $ do
        it "should return true when passed a ';' character" $ do
          checkEndOfStatement ';' `shouldBe` True

        it "should return false when passed a character other than ';'" $ do
          checkEndOfStatement 'f' `shouldBe` False

      describe "splitStringOnDelimeter" $ do
              it "should return an empty list if passed nothing" $ do
                splitStringOnDelimeter "" ',' `shouldBe` [""]

              it "should return a list that that is split on the delimiter of the string passed" $ do
                splitStringOnDelimeter "this is a sentence with spaces" ' ' `shouldBe` ["this", "is", "a", "sentence", "with", "spaces"]

              it "should return a list with one item if the delimeter is not in the string" $ do
                splitStringOnDelimeter "thisIsASentenceWithNoSpaces" ' ' `shouldBe` ["thisIsASentenceWithNoSpaces"]

      describe "getNextStatememt" $ do
        it "should return the ';' character when passed a ';' character" $ do
          (getNextStatement "" "" ";") `shouldBe` (FileParsingInformation "" ";" "")

        it "should append the ';' character when the next character is ';'" $ do
          (getNextStatement "" "HELLO WORLD" ";") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" "")

        it "should pass all values inclusively when it contains the ';' character" $ do
          (getNextStatement "" "" "HELLO WORLD;") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" "")

        it "should pass the statement back if there are no statements left to parse" $ do
          (getNextStatement "" "HELLO WORLD" "") `shouldBe` (FileParsingInformation "" "HELLO WORLD" "")

        it "should pass all values inclusively when there are no other splitting characters in the string" $ do
          (getNextStatement "" "" "HELLO WORLD;") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" "")

        it "should pass all values inclusively when it contains the ';' character" $ do
          (getNextStatement "" "" "HELLO WORLD; WORLD, HELLO") `shouldBe` (FileParsingInformation "" "HELLO WORLD;" " WORLD, HELLO")

      describe "checkEndOfStatement" $ do
        it "should return true if the character is ;" $ do
          checkEndOfStatement ';' `shouldBe` True

        it "should return false if the character is something else" $ do
          checkEndOfStatement ':' `shouldBe` False
          checkEndOfStatement 'a' `shouldBe` False
          checkEndOfStatement 'j' `shouldBe` False
          checkEndOfStatement '@' `shouldBe` False
          checkEndOfStatement '"' `shouldBe` False

      describe "(+++)" $ do
        it "concat two monad strings together" $ do
          concatOutput <- (return "firstPart") +++ (return "secondPart")
          concatOutput `shouldBe` "firstPartsecondPart"

      describe "exec" $ do
        it "Return a response if the command succeeds" $ do
          commandOutput <- exec "dir" "deletedStatement"
          commandOutput `shouldBe` "Deleting the statement deletedStatement did not fail any tests. Please look into this!\n\n"

