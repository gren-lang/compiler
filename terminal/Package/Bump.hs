{-# LANGUAGE OverloadedStrings #-}

module Package.Bump
  ( run,
    Flags (..),
  )
where

import Gren.Package qualified as Package
import Gren.ModuleName qualified as ModuleName
import Gren.Outline (Outline)
import Gren.Outline qualified as Outline
import BackgroundWriter qualified as BW
import Data.ByteString.Internal (ByteString)
import Data.Map (Map)
import Build qualified
import Data.List qualified as List
import Data.NonEmptyList qualified as NE
import Deps.Diff qualified as Diff
import Deps.Package qualified as Package
import Directories qualified as Dirs
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.Magnitude qualified as M
import Gren.Outline qualified as Outline
import Gren.Version qualified as V
import Reporting qualified
import Reporting.Doc ((<+>))
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import Reporting.Exit.Help qualified as Help
import Reporting.Task qualified as Task

-- RUN

data Flags = Flags
  { _interactive :: Bool,
    _project_path :: String,
    _known_versions :: [V.Version],
    _outline :: Outline.PkgOutline,
    _root_sources :: Map ModuleName.Raw ByteString,
    _dependencies :: Map Package.Name Details.Dependency
  }
  deriving (Show)

run :: Flags -> IO ()
run flags =
  Reporting.attempt Exit.bumpToReport $
    Task.run (bump flags)

-- BUMP

bump :: Flags -> Task.Task Exit.Bump ()
bump flags@(Flags interactive root knownVersions outline@(Outline.PkgOutline pkg _ _ vsn _ _ _ _) sources deps) =
  Task.eio id $
    case knownVersions of
      (v : vs) ->
        let bumpableVersions =
              map (\(old, _, _) -> old) (Package.bumpPossibilities (v, vs))
         in if elem vsn bumpableVersions
              then Task.run $ suggestVersion flags
              else do
                return $
                  Left $
                    Exit.BumpUnexpectedVersion vsn $
                      map head (List.group (List.sort bumpableVersions))
      [] ->
        do
          checkNewPackage flags root outline
          return $ Right ()

-- CHECK NEW PACKAGE

checkNewPackage :: Flags -> FilePath -> Outline.PkgOutline -> IO ()
checkNewPackage flags root outline@(Outline.PkgOutline _ _ _ version _ _ _ _) =
  do
    putStrLn Exit.newPackageOverview
    if version == V.one
      then putStrLn "The version number in gren.json is correct so you are all set!"
      else
        changeVersion flags root outline V.one $
          "It looks like the version in gren.json has been changed though!\n\
          \Would you like me to change it back to "
            <> D.fromVersion V.one
            <> "? [Y/n] "

-- SUGGEST VERSION

suggestVersion :: Flags -> Task.Task Exit.Bump ()
suggestVersion flags@(Flags interactive root _ outline@(Outline.PkgOutline pkg _ _ vsn _ _ _ _) sources deps) =
  do
    -- TODO: Temporary until Diff.getDocs is rewritten
    -- oldDocs <-
    --   Task.mapError
    --     (Exit.BumpCannotFindDocs pkg vsn)
    --     (Diff.getDocs cache pkg vsn)

    oldDocs <- generateDocs root outline
    newDocs <- generateDocs root outline
    let changes = Diff.diff oldDocs newDocs
    let newVersion = Diff.bump changes vsn
    Task.io $
      changeVersion flags root outline newVersion $
        let old = D.fromVersion vsn
            new = D.fromVersion newVersion
            mag = D.fromChars $ M.toChars (Diff.toMagnitude changes)
         in "Based on your new API, this should be a"
              <+> D.green mag
              <+> "change ("
              <> old
              <> " => "
              <> new
              <> ")\n"
              <> "Bail out of this command and run 'gren diff' for a full explanation.\n"
              <> "\n"
              <> "Should I perform the update ("
              <> old
              <> " => "
              <> new
              <> ") in gren.json? [Y/n] "

generateDocs :: FilePath -> Outline.PkgOutline -> Task.Task Exit.Bump Docs.Documentation
generateDocs root (Outline.PkgOutline _ _ _ _ exposed _ _ _) =
  do
    details <-
      Task.eio Exit.BumpBadDetails $
        BW.withScope $ \scope ->
          Details.load Reporting.silent scope root

    case Outline.flattenExposed exposed of
      [] ->
        Task.throw Exit.BumpNoExposed
      e : es ->
        Task.eio Exit.BumpBadBuild $
          Build.fromExposed Reporting.silent root details Build.KeepDocs (NE.List e es)

-- CHANGE VERSION

changeVersion :: Flags -> FilePath -> Outline.PkgOutline -> V.Version -> D.Doc -> IO ()
changeVersion flags root outline targetVersion question =
  do
    approved <- Reporting.ask (not $ _interactive flags) question
    if not approved
      then putStrLn "Okay, I did not change anything!"
      else do
        Outline.write root $
          Outline.Pkg $
            outline {Outline._pkg_version = targetVersion}

        Help.toStdout $
          "Version changed to "
            <> D.green (D.fromVersion targetVersion)
            <> "!\n"
