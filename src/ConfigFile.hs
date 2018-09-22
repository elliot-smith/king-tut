module ConfigFile
(readConfig,
ConfigInfo  (..)) where


import Data.ConfigFile
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

data ConfigInfo = ConfigInfo { srcFilePath :: String,
                               testingFileName :: String,
                               inputTestCommand :: String }

--  Credits to http://tuttlem.github.io/2013/07/04/configfile-basics-in-haskell.html
readConfig :: String -> IO ConfigInfo
readConfig f = do
   rv <- runExceptT $ do

      -- open the configuration file
      cp <- join $ liftIO $ readfile emptyCP f
      let x = cp

      -- read out the attributes
      path <- get x "Default" "path"
      fn <- get x "Default" "filename"
      command <- get x "Default" "testcommand"

      -- build the config value
      return (ConfigInfo { srcFilePath = path, testingFileName = fn, inputTestCommand = command })

   -- in the instance that configuration reading failed we'll
   -- fail the application here, otherwise send out the config
   -- value that we've built
   either (\x -> error (snd x)) (\x -> return x) rv