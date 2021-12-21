module Git
    ( checkInstalledGit
    )
    where


import System.Directory (findExecutable)


checkInstalledGit :: IO Bool
checkInstalledGit = do
   gitPath <- findExecutable "git"
   return $ case gitPath of
     Just _ -> True
     Nothing -> False

