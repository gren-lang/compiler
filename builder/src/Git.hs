module Git
    ( GitUrl
    , checkInstalledGit
    , githubUrl
    , clone
    , localClone
    , update
    , tags
    )
    where

import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Parse.Primitives as Parser

import System.Directory (findExecutable)
import qualified System.IO as IO
import qualified System.Process as Process
import qualified Data.Either as Either
import qualified Data.ByteString.Char8 as BS


checkInstalledGit :: IO Bool
checkInstalledGit = do
   gitPath <- findExecutable "git"
   return $ case gitPath of
     Just _ -> True
     Nothing -> False


newtype GitUrl
  = GitUrl String

githubUrl :: Pkg.Name -> GitUrl
githubUrl pkg =
    GitUrl $ "https://github.com/" ++ Pkg.toUrl pkg ++ ".git"


clone :: GitUrl -> FilePath -> IO ()
clone (GitUrl gitUrl) targetFolder = do
    nullHandle <- IO.openFile "/dev/null" IO.WriteMode
    procResult <-
        Process.createProcess
            (Process.proc "git" [ "clone" , "--bare", gitUrl, targetFolder ])
            { Process.std_out = Process.UseHandle nullHandle
            , Process.std_err = Process.UseHandle nullHandle
            }
    return ()


localClone :: FilePath -> V.Version -> FilePath -> IO ()
localClone gitUrl vsn targetFolder = do
    nullHandle <- IO.openFile "/dev/null" IO.WriteMode
    procResult <- 
        Process.createProcess 
            (Process.proc "git" 
                [ "clone"
                , gitUrl
                , "--local"
                , "-b", V.toChars vsn
                , "--depth", "1"
                , targetFolder 
                ]) 
            { Process.std_out = Process.UseHandle nullHandle
            , Process.std_err = Process.UseHandle nullHandle
            }
    return ()


update :: FilePath -> IO ()
update path = do
    nullHandle <- IO.openFile "/dev/null" IO.WriteMode
    procResult <-
        Process.createProcess
            (Process.proc "git" [ "pull", "--tags" ])
            { Process.std_out = Process.UseHandle nullHandle
            , Process.std_err = Process.UseHandle nullHandle
            }
    return ()


tags :: FilePath -> IO (Maybe (V.Version, [V.Version]))
tags path = do
    (exitCode, stdout, stderr) <- Process.readProcessWithExitCode "git" [ "tag" ] ""
    let tags = map BS.pack $ lines stdout
    let versions = Either.rights $ map (Parser.fromByteString V.parser (,)) tags
    case versions of
      [] -> return Nothing
      v:vs -> return $ Just (v, vs)
    
