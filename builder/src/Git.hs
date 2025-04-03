module Git
  ( GitUrl,
    Error (..),
    --
    githubUrl,
    clone,
    tags,
    --
    hasLocalTag,
    hasLocalChangesSinceTag,
  )
where

import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import System.Directory (findExecutable)
import System.Exit qualified as Exit
import System.IO qualified as IO
import System.Process qualified as Process

data Error
  = MissingGit
  | NoVersions
  | NoSuchRepo
  | NoSuchRepoOrVersion V.Version
  | FailedCommand [String] String

--

checkInstalledGit :: IO (Maybe FilePath)
checkInstalledGit =
  findExecutable "git"

putStrFlush :: String -> IO ()
putStrFlush str =
  IO.putStr str >> IO.hFlush IO.stdout

--

newtype GitUrl
  = GitUrl (String, String)

githubUrl :: Pkg.Name -> GitUrl
githubUrl pkg =
  GitUrl
    ( Pkg.toChars pkg,
      "https://github.com/" ++ Pkg.toUrl pkg ++ ".git"
    )

--

clone :: GitUrl -> V.Version -> FilePath -> IO (Either Error ())
clone (GitUrl (_, gitUrl)) vsn targetFolder = do
  maybeExec <- checkInstalledGit
  case maybeExec of
    Nothing ->
      return $ Left MissingGit
    Just git -> do
      let args =
            [ "clone",
              "--branch",
              V.toChars vsn,
              "--depth",
              "1",
              gitUrl,
              targetFolder
            ]
      (exitCode, _, stderr) <-
        Process.readCreateProcessWithExitCode
          (Process.proc git args)
          ""
      case exitCode of
        Exit.ExitFailure 128 -> do
          return $ Left $ NoSuchRepoOrVersion vsn
        Exit.ExitFailure _ -> do
          return $ Left $ FailedCommand ("git" : args) stderr
        Exit.ExitSuccess -> do
          return $ Right ()

tags :: GitUrl -> IO (Either Error (V.Version, [V.Version]))
tags (GitUrl (pkgName, gitUrl)) = do
  maybeExec <- checkInstalledGit
  putStrFlush $ "Retrieving versions for " ++ pkgName ++ "... "
  case maybeExec of
    Nothing ->
      return $ Left MissingGit
    Just git -> do
      let args =
            [ "ls-remote",
              "--tags",
              gitUrl
            ]
      (exitCode, _, stderr) <-
        Process.readCreateProcessWithExitCode
          (Process.proc git args)
          ""
      case exitCode of
        Exit.ExitFailure 128 -> do
          putStrLn "Error!"
          return $ Left NoSuchRepo
        Exit.ExitFailure _ -> do
          putStrLn "Error!"
          return $ Left $ FailedCommand ("git" : args) stderr
        Exit.ExitSuccess ->
          do
            putStrLn "!This should no longer be in use!"
            return $ Left NoVersions

--

hasLocalTag :: V.Version -> IO (Either Error ())
hasLocalTag vsn = do
  maybeExec <- checkInstalledGit
  case maybeExec of
    Nothing ->
      return $ Left MissingGit
    Just git -> do
      let args = ["show", "--name-only", V.toChars vsn, "--"]
      (exitCode, _, stderr) <-
        Process.readCreateProcessWithExitCode
          (Process.proc git args)
          ""
      case exitCode of
        Exit.ExitFailure _ -> do
          return $ Left $ FailedCommand ("git" : args) stderr
        Exit.ExitSuccess ->
          return $ Right ()

hasLocalChangesSinceTag :: V.Version -> IO (Either Error ())
hasLocalChangesSinceTag vsn = do
  maybeExec <- checkInstalledGit
  case maybeExec of
    Nothing ->
      return $ Left MissingGit
    Just git -> do
      let args = ["diff-index", "--quiet", V.toChars vsn, "--"]
      (exitCode, _, stderr) <-
        Process.readCreateProcessWithExitCode
          (Process.proc git args)
          ""
      case exitCode of
        Exit.ExitFailure _ -> do
          return $ Left $ FailedCommand ("git" : args) stderr
        Exit.ExitSuccess ->
          return $ Right ()
