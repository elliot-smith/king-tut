# king-tut



## Important commands
1. Run the main file Main -> `stack build && stack runhaskell app/main`
1. Test the application -> `stack tests`
1. Build the Haskell stack -> `stack build`
1. Watch files and check semantics -> `stack build --file-watch --pedantic`

This application is designed to Test Ur Tests. Code coverage unfortunately only checks whether a statement was executed
during your tests but King Tut is designed to test that each line of code is tested. If it is not tested, then it will
produce an output file called `king-tut-output.txt` that will contain all lines which were not covered.

### Configuration file

Please read the documentation here for information on how to use a config file with King Tut: http://hackage.haskell.org/package/ConfigFile-1.1.4/docs/Data-ConfigFile.html

TODO:
1. Modify original file and test (completed!)
2. Select a file from a list of available options (i.e. Glob)
3. Use an environment file to pass through all variables (completed!)
4. Modify test files to focus only on that file
5. Pattern match on newline that is not part of the previous expression
6. Continue Pattern matching algos
7. Ignore statements from config file