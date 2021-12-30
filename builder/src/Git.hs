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
    putStrLn $ "Cloning " ++ gitUrl
    procResult <-
        Process.readCreateProcessWithExitCode
            (Process.proc "git" [ "clone" , "--bare", gitUrl, targetFolder ])
            ""
    return ()


localClone :: FilePath -> V.Version -> FilePath -> IO ()
localClone gitUrl vsn targetFolder = do
    putStrLn $ "Checking out " ++ gitUrl ++ " version " ++ V.toChars vsn
    procResult <- 
        Process.readCreateProcessWithExitCode
            (Process.proc "git" 
                [ "clone"
                , gitUrl
                , "--local"
                , "-b", V.toChars vsn
                , "--depth", "1"
                , targetFolder 
                ]) 
            ""
    return ()


update :: FilePath -> IO ()
update path = do
    putStrLn $ "Updating " ++ path
    procResult <-
        Process.readCreateProcessWithExitCode
            ((Process.proc "git" [ "pull", "--tags" ])
            { Process.cwd = Just path })
            ""
    return ()


tags :: FilePath -> IO (Maybe (V.Version, [V.Version]))
tags path = do
    (exitCode, stdout, stderr) <-
        Process.readCreateProcessWithExitCode
            ((Process.proc "git" [ "tag" ])
            { Process.cwd = Just path })
            ""
    let tags = map BS.pack $ lines stdout
    let versions = Either.rights $ map (Parser.fromByteString V.parser (,)) tags
    case versions of
      [] -> return Nothing
      v:vs -> return $ Just (v, vs)
    
