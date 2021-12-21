module Git
    ( checkInstalledGit
    , clone
    )
    where


import System.Directory (findExecutable)
import qualified System.IO as IO
import qualified System.Process as Process


checkInstalledGit :: IO Bool
checkInstalledGit = do
   gitPath <- findExecutable "git"
   return $ case gitPath of
     Just _ -> True
     Nothing -> False


clone :: String -> String -> IO ()
clone targetFolder gitUrl = do
    nullHandle <- IO.openFile "/dev/null" IO.WriteMode
    procResult <- 
        Process.createProcess (Process.proc "git" [ "clone", gitUrl, targetFolder ]) 
            { Process.std_out = Process.UseHandle nullHandle
            , Process.std_err = Process.UseHandle nullHandle
            }
    return ()
