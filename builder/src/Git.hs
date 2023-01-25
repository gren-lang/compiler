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
    --
    kernelCodeSignedByLeadDeveloper,
  )
where

import Data.Binary.Get qualified as Get
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder qualified as BSBuilder
import Data.ByteString.Char8 qualified as BSStrict
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BSLazy
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
      (exitCode, stdout, stderr) <-
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

--

kernelCodePublicKey :: String
kernelCodePublicKey =
  "7373682d65643235353139000000204138f9d16b668718e47b4628c85a852434d79cc005fc5e7388"

kernelCodeSignedByLeadDeveloper :: FilePath -> IO Bool
kernelCodeSignedByLeadDeveloper path = do
  maybeExec <- checkInstalledGit
  case maybeExec of
    Nothing ->
      return False
    Just git -> do
      jsFilesUnchanged <- noChangesToJSFilesSinceHead git path
      if not jsFilesUnchanged
        then return False
        else do
          commitHash <- lastCommitWithChangesToJSFile git path
          publicKey <- extractPublicKeyFromCommit git path commitHash
          return $ publicKey == kernelCodePublicKey

noChangesToJSFilesSinceHead :: FilePath -> FilePath -> IO Bool
noChangesToJSFilesSinceHead git path = do
  let args = ["diff-index", "--quiet", "HEAD", "--", "*.js"]
  (exitCode, _, _) <-
    Process.readCreateProcessWithExitCode
      (Process.proc git args) {Process.cwd = Just path}
      ""
  case exitCode of
    Exit.ExitFailure _ ->
      return False
    Exit.ExitSuccess ->
      return True

lastCommitWithChangesToJSFile :: FilePath -> FilePath -> IO String
lastCommitWithChangesToJSFile git path = do
  let args = ["log", "-n 1", "--format=%H", "--", "*.js"]
  (exitCode, stdout, _) <-
    Process.readCreateProcessWithExitCode
      (Process.proc git args) {Process.cwd = Just path}
      ""
  case exitCode of
    Exit.ExitFailure _ ->
      return ""
    Exit.ExitSuccess ->
      -- trim = unwords . words
      return (unwords (words stdout))

extractPublicKeyFromCommit :: FilePath -> FilePath -> String -> IO String
extractPublicKeyFromCommit git path hash = do
  let args = ["cat-file", "-p", hash]
  (exitCode, stdout, _) <-
    Process.readCreateProcessWithExitCode
      (Process.proc git args) {Process.cwd = Just path}
      ""
  case exitCode of
    Exit.ExitFailure _ -> do
      return ""
    Exit.ExitSuccess ->
      let decodedSignatureChunk =
            Base64.decode $
              BSStrict.pack $
                concatMap (dropWhile (\c -> c == ' ')) $
                  takeWhile (\line -> not $ List.isInfixOf "-----END SSH SIGNATURE-----" line) $
                    drop 1 $
                      dropWhile (\line -> not $ List.isPrefixOf "gpgsig" line) $
                        lines stdout
       in case decodedSignatureChunk of
            Left err ->
              return err
            Right decoded ->
              return $
                BSLazy.unpack $
                  BSBuilder.toLazyByteString $
                    BSBuilder.lazyByteStringHex $
                      Get.runGet decodePublicKeyFromChunk $
                        BSLazy.fromStrict decoded

{- Description of format at https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.sshsig
  Description of byte encoding at https://dl.acm.org/doi/pdf/10.17487/RFC4253
-}
decodePublicKeyFromChunk :: Get.Get ByteString
decodePublicKeyFromChunk = do
  Get.skip 6
  _version <- Get.getInt32be
  publicKeyLen <- Get.getInt32be
  publicKeyPadding <- Get.getInt32be
  let actualPublicKeyLen = fromIntegral (publicKeyLen - publicKeyPadding)
  Get.getLazyByteString actualPublicKeyLen
