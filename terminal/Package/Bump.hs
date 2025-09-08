{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Package.Bump
  ( run,
    Flags (..),
  )
where

import Build qualified
import Command qualified
import Data.List qualified as List
import Data.Map (Map)
import Data.NonEmptyList qualified as NE
import Deps.Diff qualified as Diff
import Deps.Package qualified as Package
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.Magnitude qualified as M
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
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
    _current_version :: Command.ProjectInfo,
    _published_version :: Command.ProjectInfo
  }
  deriving (Show)

run :: Flags -> IO ()
run flags@(Flags _ _ _ currentVersion publishedVersion) =
  Reporting.attempt Exit.bumpToReport $
    case (Command._project_outline currentVersion, Command._project_outline publishedVersion) of
      (Outline.Pkg currentPackage, Outline.Pkg publishedPkg) ->
        Task.run (bump flags currentPackage publishedPkg)
      _ ->
        error "Received outlines are in the wrong format"

-- BUMP

bump :: Flags -> Outline.PkgOutline -> Outline.PkgOutline -> Task.Task Exit.Bump ()
bump flags@(Flags _ _ knownVersions _ _) currentOutline@(Outline.PkgOutline _ _ _ vsn _ _ _ _) publishedOutline =
  Task.eio id $
    case reverse knownVersions of
      (v : vs) ->
        let bumpableVersions =
              map (\(old, _, _) -> old) (Package.bumpPossibilities (v, vs))
         in if elem vsn bumpableVersions
              then Task.run $ suggestVersion flags currentOutline publishedOutline
              else do
                return $
                  Left $
                    Exit.BumpUnexpectedVersion vsn $
                      map head (List.group (List.sort bumpableVersions))
      [] ->
        error "known versions was empty"

-- SUGGEST VERSION

suggestVersion :: Flags -> Outline.PkgOutline -> Outline.PkgOutline -> Task.Task Exit.Bump ()
suggestVersion flags@(Flags _ root _ (Command.ProjectInfo _ currentSources currentDeps) (Command.ProjectInfo _ publishedSources publishedDeps)) currentOutline@(Outline.PkgOutline _ _ _ vsn _ _ _ _) publishedOutline =
  do
    newDocs <- generateDocs root currentOutline currentSources currentDeps
    oldDocs <- generateDocs root publishedOutline publishedSources publishedDeps
    let changes = Diff.diff oldDocs newDocs
    let newVersion = Diff.bump changes vsn
    Task.io $
      changeVersion flags root currentOutline newVersion $
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

generateDocs :: FilePath -> Outline.PkgOutline -> Build.Sources -> Map Pkg.Name Details.Dependency -> Task.Task Exit.Bump Docs.Documentation
generateDocs root outline@(Outline.PkgOutline _ _ _ _ exposed _ _ _) sources solution =
  do
    details <-
      Task.eio Exit.BumpBadDetails $
        Details.load (Outline.Pkg outline) solution

    case Outline.flattenExposed exposed of
      [] ->
        Task.throw Exit.BumpNoExposed
      e : es ->
        Task.eio Exit.BumpBadBuild $
          Build.fromExposed Reporting.silent root details sources Build.KeepDocs (NE.List e es)

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
