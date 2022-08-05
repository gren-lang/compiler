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

import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Parse.Primitives qualified as Parser
import System.Directory (findExecutable)
import System.Exit qualified as Exit
import System.IO qualified as IO
import System.Process qualified as Process

data Error
  = MissingGit
  | FailedCommand (Maybe FilePath) [String] String
  | NoVersions

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
clone (GitUrl (pkgName, gitUrl)) vsn targetFolder = do
  maybeExec <- checkInstalledGit
  putStrFlush $ "Cloning " ++ pkgName ++ " " ++ V.toChars vsn ++ "... "
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
        Exit.ExitFailure _ -> do
          putStrLn "Error!"
          return $ Left $ FailedCommand Nothing ("git" : args) stderr
        Exit.ExitSuccess -> do
          putStrLn "Ok!"
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
      (exitCode, stdout, stderr) <-
        Process.readCreateProcessWithExitCode
          (Process.proc git args)
          ""
      case exitCode of
        Exit.ExitFailure _ -> do
          putStrLn "Error!"
          return $ Left $ FailedCommand Nothing ("git" : args) stderr
        Exit.ExitSuccess ->
          let tagList =
                map (TE.encodeUtf8) $
                  map (Text.replace (Text.pack "refs/tags/") Text.empty) $
                    map (Text.pack) $
                      map (Maybe.fromMaybe "" . listGet 1) $
                        map words $
                          lines stdout

              versions =
                reverse $
                  List.sort $
                    Either.rights $ -- Ignore tags that aren't semantic versions
                      map (Parser.fromByteString V.parser (,)) tagList
           in do
                putStrLn "Ok!"
                return $ case versions of
                  [] -> Left NoVersions
                  v : vs -> Right (v, vs)

listGet :: Int -> [a] -> Maybe a
listGet idx ls =
  case ls of
    [] -> Nothing
    first : rest ->
      if idx == 0
        then Just first
        else listGet (idx - 1) rest

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
          return $ Left $ FailedCommand Nothing ("git" : args) stderr
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
          return $ Left $ FailedCommand Nothing ("git" : args) stderr
        Exit.ExitSuccess ->
          return $ Right ()
