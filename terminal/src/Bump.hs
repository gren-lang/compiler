{-# LANGUAGE OverloadedStrings #-}

module Bump
  ( run,
  )
where

import BackgroundWriter qualified as BW
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

run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.bumpToReport $
    Task.run (bump =<< getEnv)

-- ENV

data Env = Env
  { _root :: FilePath,
    _cache :: Dirs.PackageCache,
    _outline :: Outline.PkgOutline
  }

getEnv :: Task.Task Exit.Bump Env
getEnv =
  do
    maybeRoot <- Task.io Dirs.findRoot
    case maybeRoot of
      Nothing ->
        Task.throw Exit.BumpNoOutline
      Just root ->
        do
          cache <- Task.io Dirs.getPackageCache
          outline <- Task.eio Exit.BumpBadOutline $ Outline.read root
          case outline of
            Outline.App _ ->
              Task.throw Exit.BumpApplication
            Outline.Pkg pkgOutline ->
              return $ Env root cache pkgOutline

-- BUMP

bump :: Env -> Task.Task Exit.Bump ()
bump env@(Env root _ outline@(Outline.PkgOutline pkg _ _ vsn _ _ _ _)) =
  Task.eio id $
    do
      versionResult <- Package.getVersions pkg
      case versionResult of
        Right knownVersions ->
          let bumpableVersions =
                map (\(old, _, _) -> old) (Package.bumpPossibilities knownVersions)
           in if elem vsn bumpableVersions
                then Task.run $ suggestVersion env
                else do
                  return $
                    Left $
                      Exit.BumpUnexpectedVersion vsn $
                        map head (List.group (List.sort bumpableVersions))
        Left _ ->
          do
            checkNewPackage root outline
            return $ Right ()

-- CHECK NEW PACKAGE

checkNewPackage :: FilePath -> Outline.PkgOutline -> IO ()
checkNewPackage root outline@(Outline.PkgOutline _ _ _ version _ _ _ _) =
  do
    putStrLn Exit.newPackageOverview
    if version == V.one
      then putStrLn "The version number in gren.json is correct so you are all set!"
      else
        changeVersion root outline V.one $
          "It looks like the version in gren.json has been changed though!\n\
          \Would you like me to change it back to "
            <> D.fromVersion V.one
            <> "? [Y/n] "

-- SUGGEST VERSION

suggestVersion :: Env -> Task.Task Exit.Bump ()
suggestVersion (Env root cache outline@(Outline.PkgOutline pkg _ _ vsn _ _ _ _)) =
  do
    oldDocs <-
      Task.mapError
        (Exit.BumpCannotFindDocs pkg vsn)
        (Diff.getDocs cache pkg vsn)

    newDocs <- generateDocs root outline
    let changes = Diff.diff oldDocs newDocs
    let newVersion = Diff.bump changes vsn
    Task.io $
      changeVersion root outline newVersion $
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

changeVersion :: FilePath -> Outline.PkgOutline -> V.Version -> D.Doc -> IO ()
changeVersion root outline targetVersion question =
  do
    approved <- Reporting.ask question
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
