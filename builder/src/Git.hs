module Git
    ( checkInstalledGit
    , githubUrl
    , clone
    )
    where

import qualified Elm.Package as Pkg
import qualified Elm.Version as V

import System.Directory (findExecutable)
import qualified System.IO as IO
import qualified System.Process as Process


checkInstalledGit :: IO Bool
checkInstalledGit = do
   gitPath <- findExecutable "git"
   return $ case gitPath of
     Just _ -> True
     Nothing -> False


githubUrl :: Pkg.Name -> String
githubUrl pkg =
    "https://github.com/" ++ Pkg.toUrl pkg ++ ".git"


clone :: String -> V.Version -> String -> IO ()
clone gitUrl vsn targetFolder = do
    nullHandle <- IO.openFile "/dev/null" IO.WriteMode
    procResult <- 
        Process.createProcess 
            (Process.proc "git" 
                [ "clone"
                , gitUrl
                , "-b", V.toChars vsn
                , "--depth", "1"
                , targetFolder 
                ]) 
            { Process.std_out = Process.UseHandle nullHandle
            , Process.std_err = Process.UseHandle nullHandle
            }
    return ()
