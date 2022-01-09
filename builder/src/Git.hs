module Git
    ( GitUrl
    , Problem(..)
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
import qualified Reporting as R

import System.Directory (findExecutable)
import qualified System.IO as IO
import qualified System.Process as Process
import qualified System.Exit as Exit
import qualified Data.Either as Either
import qualified Data.ByteString.Char8 as BS


data Problem
  = MissingGit
  | FailedCommand (Maybe FilePath) [String] String
  | NoVersions FilePath


--


checkInstalledGit :: IO (Maybe FilePath)
checkInstalledGit =
  findExecutable "git"


--


newtype GitUrl
  = GitUrl (String, String)


githubUrl :: Pkg.Name -> GitUrl
githubUrl pkg =
    GitUrl
      ( Pkg.toChars pkg
      , "https://github.com/" ++ Pkg.toUrl pkg ++ ".git"
      )


--


clone :: GitUrl -> FilePath -> IO (Either Problem ())
clone (GitUrl (pkgName, gitUrl)) targetFolder = do
    maybeExec <- checkInstalledGit
    R.putStrFlush $ "Cloning " ++ pkgName ++ "... "
    case maybeExec of
      Nothing -> do
          putStrLn "Error!"
          return $ Left MissingGit

      Just git -> do
        let args = [ "clone" , "--bare", gitUrl, targetFolder ]
        (exitCode, _, stderr) <-
            Process.readCreateProcessWithExitCode
                (Process.proc "git" args)
                ""
        case exitCode of
          Exit.ExitFailure _ -> do
            putStrLn "Error!"
            return $ Left $ FailedCommand Nothing ("git":args) stderr

          Exit.ExitSuccess -> do
            putStrLn "Ok!"
            return $ Right ()


localClone :: FilePath -> V.Version -> FilePath -> IO (Either Problem ())
localClone gitUrl vsn targetFolder = do
    maybeExec <- checkInstalledGit
    case maybeExec of
      Nothing ->
        return $ Left MissingGit

      Just git -> do
        let args = [ "clone"
                   , gitUrl
                   , "--local"
                   , "-b", V.toChars vsn
                   , "--depth", "1"
                   , targetFolder
                   ]
        (exitCode, _, stderr) <-
            Process.readCreateProcessWithExitCode
                (Process.proc git args)
                ""
        case exitCode of
          Exit.ExitFailure _ -> do
            putStrLn "Error!"
            return $ Left $ FailedCommand Nothing ("git":args) stderr

          Exit.ExitSuccess ->
            return $ Right ()


update :: Pkg.Name -> FilePath -> IO (Either Problem ())
update pkg path = do
    maybeExec <- checkInstalledGit
    R.putStrFlush $ "Updating " ++ Pkg.toChars pkg ++ "... "
    case maybeExec of
      Nothing -> do
        putStrLn "Error!"
        return $ Left MissingGit

      Just git -> do
        let args = [ "fetch" ]
        (exitCode, _, stderr) <-
            Process.readCreateProcessWithExitCode
                ((Process.proc git args)
                { Process.cwd = Just path })
                ""
        case exitCode of
          Exit.ExitFailure _ -> do
            putStrLn "Error!"
            return $ Left $ FailedCommand (Just path) ("git":args) stderr

          Exit.ExitSuccess -> do
            putStrLn "Ok!"
            return $ Right ()


tags :: FilePath -> IO (Either Problem (V.Version, [V.Version]))
tags path = do
    maybeExec <- checkInstalledGit
    case maybeExec of
      Nothing ->
        return $ Left MissingGit

      Just git -> do
        let args = [ "tag" ]
        (exitCode, stdout, stderr) <-
            Process.readCreateProcessWithExitCode
                ((Process.proc git args)
                { Process.cwd = Just path })
                ""
        case exitCode of
          Exit.ExitFailure _ -> do
            putStrLn "Error!"
            return $ Left $ FailedCommand (Just path) ("git":args) stderr

          Exit.ExitSuccess ->
            let
              tags =
                map BS.pack $ lines stdout

              -- Ignore tags that aren't semantic versions
              versions =
                Either.rights $ map (Parser.fromByteString V.parser (,)) tags
            in
            case versions of
              [] -> return $ Left $ NoVersions path
              v:vs -> return $ Right (v, vs)
    
