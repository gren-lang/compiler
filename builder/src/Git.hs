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


-- IO Helper


putStrFlush :: String -> IO ()
putStrFlush str =
  putStr str >> IO.hFlush IO.stdout


--


checkInstalledGit :: IO Bool
checkInstalledGit = do
   gitPath <- findExecutable "git"
   return $ case gitPath of
     Just _ -> True
     Nothing -> False


newtype GitUrl
  = GitUrl (String, String)


githubUrl :: Pkg.Name -> GitUrl
githubUrl pkg =
    GitUrl
      ( Pkg.toChars pkg
      , "https://github.com/" ++ Pkg.toUrl pkg ++ ".git"
      )


clone :: GitUrl -> FilePath -> IO ()
clone (GitUrl (pkgName, gitUrl)) targetFolder = do
    putStrFlush $ "Cloning " ++ pkgName ++ "... "
    procResult <-
        Process.readCreateProcessWithExitCode
            (Process.proc "git" [ "clone" , "--bare", gitUrl, targetFolder ])
            ""
    putStrLn "Done!"
    return ()


localClone :: FilePath -> V.Version -> FilePath -> IO ()
localClone gitUrl vsn targetFolder = do
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


update :: Pkg.Name -> FilePath -> IO ()
update pkg path = do
    putStrFlush $ "Updating " ++ Pkg.toChars pkg ++ "... "
    procResult <-
        Process.readCreateProcessWithExitCode
            ((Process.proc "git" [ "pull", "--tags" ])
            { Process.cwd = Just path })
            ""
    putStrLn "Done!"
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
    
