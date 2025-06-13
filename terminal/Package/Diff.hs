{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Package.Diff
  ( Flags (..),
    run,
  )
where

import Build qualified
import Command qualified
import Data.ByteString.Internal (ByteString)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as Name
import Data.NonEmptyList qualified as NE
import Deps.Diff (Changes (..), ModuleChanges (..), PackageChanges (..))
import Deps.Diff qualified as DD
import Gren.Compiler.Type qualified as Type
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.Magnitude qualified as M
import Gren.ModuleName qualified as ModuleName
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Reporting qualified
import Reporting.Doc ((<+>))
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import Reporting.Exit.Help qualified as Help
import Reporting.Render.Type.Localizer qualified as L
import Reporting.Task qualified as Task

-- RUN

data Flags = Flags
  { _interactive :: Bool,
    _project_path :: String,
    _current_version :: Command.ProjectInfo,
    _published_version :: Command.ProjectInfo
  }
  deriving (Show)

run :: Flags -> IO ()
run flags@(Flags _ _ firstPackage secondPackage) =
  Reporting.attempt Exit.diffToReport $
    case (Command._project_outline firstPackage, Command._project_outline secondPackage) of
      (Outline.Pkg firstOutline, Outline.Pkg secondOutline) ->
        Task.run (diff flags firstOutline secondOutline)
      _ ->
        error "Received outlines are in the wrong format"

-- DIFF

type Task a =
  Task.Task Exit.Diff a

diff :: Flags -> Outline.PkgOutline -> Outline.PkgOutline -> Task ()
diff (Flags _ root (Command.ProjectInfo _ firstSources firstSolution) (Command.ProjectInfo _ secondSources secondSolution)) firstOutline secondOutline =
  do
    oldDocs <- generateDocs root firstOutline firstSources firstSolution
    newDocs <- generateDocs root secondOutline secondSources secondSolution
    writeDiff oldDocs newDocs

-- GENERATE DOCS

generateDocs :: FilePath -> Outline.PkgOutline -> Map ModuleName.Raw ByteString -> Map Pkg.Name Details.Dependency -> Task.Task Exit.Diff Docs.Documentation
generateDocs root outline@(Outline.PkgOutline _ _ _ _ exposed _ _ _) sources solution =
  do
    details <-
      Task.eio Exit.DiffBadDetails $
        Details.load Reporting.silent (Outline.Pkg outline) solution

    case Outline.flattenExposed exposed of
      [] ->
        Task.throw Exit.DiffNoExposed
      e : es ->
        Task.eio Exit.DiffBadBuild $
          Build.fromExposed Reporting.silent root details sources Build.KeepDocs (NE.List e es)

-- WRITE DIFF

writeDiff :: Docs.Documentation -> Docs.Documentation -> Task ()
writeDiff oldDocs newDocs =
  let changes = DD.diff oldDocs newDocs
      localizer = L.fromNames (Map.union oldDocs newDocs)
   in Task.io $ Help.toStdout $ toDoc localizer changes <> "\n"

-- TO DOC

toDoc :: L.Localizer -> PackageChanges -> D.Doc
toDoc localizer changes@(PackageChanges added changed removed) =
  if null added && Map.null changed && null removed
    then "No API changes detected, so this is a" <+> D.green "PATCH" <+> "change."
    else
      let magDoc =
            D.fromChars (M.toChars (DD.toMagnitude changes))

          header =
            "This is a" <+> D.green magDoc <+> "change."

          addedChunk =
            if null added
              then []
              else
                [ Chunk "ADDED MODULES" M.MINOR $
                    D.vcat $
                      map D.fromName added
                ]

          removedChunk =
            if null removed
              then []
              else
                [ Chunk "REMOVED MODULES" M.MAJOR $
                    D.vcat $
                      map D.fromName removed
                ]

          chunks =
            addedChunk ++ removedChunk ++ map (changesToChunk localizer) (Map.toList changed)
       in D.vcat (header : "" : map chunkToDoc chunks)

data Chunk = Chunk
  { _title :: String,
    _magnitude :: M.Magnitude,
    _details :: D.Doc
  }

chunkToDoc :: Chunk -> D.Doc
chunkToDoc (Chunk title magnitude details) =
  let header =
        "----" <+> D.fromChars title <+> "-" <+> D.fromChars (M.toChars magnitude) <+> "----"
   in D.vcat
        [ D.dullcyan header,
          "",
          D.indent 4 details,
          "",
          ""
        ]

changesToChunk :: L.Localizer -> (Name.Name, ModuleChanges) -> Chunk
changesToChunk localizer (name, changes@(ModuleChanges unions aliases values binops)) =
  let magnitude =
        DD.moduleChangeMagnitude changes

      (unionAdd, unionChange, unionRemove) =
        changesToDocTriple (unionToDoc localizer) unions

      (aliasAdd, aliasChange, aliasRemove) =
        changesToDocTriple (aliasToDoc localizer) aliases

      (valueAdd, valueChange, valueRemove) =
        changesToDocTriple (valueToDoc localizer) values

      (binopAdd, binopChange, binopRemove) =
        changesToDocTriple (binopToDoc localizer) binops
   in Chunk (Name.toChars name) magnitude $
        D.vcat $
          List.intersperse "" $
            Maybe.catMaybes $
              [ changesToDoc "Added" unionAdd aliasAdd valueAdd binopAdd,
                changesToDoc "Removed" unionRemove aliasRemove valueRemove binopRemove,
                changesToDoc "Changed" unionChange aliasChange valueChange binopChange
              ]

changesToDocTriple :: (k -> v -> D.Doc) -> Changes k v -> ([D.Doc], [D.Doc], [D.Doc])
changesToDocTriple entryToDoc (Changes added changed removed) =
  let indented (name, value) =
        D.indent 4 (entryToDoc name value)

      diffed (name, (oldValue, newValue)) =
        D.vcat
          [ "  - " <> entryToDoc name oldValue,
            "  + " <> entryToDoc name newValue,
            ""
          ]
   in ( map indented (Map.toList added),
        map diffed (Map.toList changed),
        map indented (Map.toList removed)
      )

changesToDoc :: String -> [D.Doc] -> [D.Doc] -> [D.Doc] -> [D.Doc] -> Maybe D.Doc
changesToDoc categoryName unions aliases values binops =
  if null unions && null aliases && null values && null binops
    then Nothing
    else
      Just $
        D.vcat $
          D.fromChars categoryName <> ":" : unions ++ aliases ++ binops ++ values

unionToDoc :: L.Localizer -> Name.Name -> Docs.Union -> D.Doc
unionToDoc localizer name (Docs.Union _ tvars ctors) =
  let setup =
        "type" <+> D.fromName name <+> D.hsep (map D.fromName tvars)

      ctorDoc (ctor, tipes) =
        typeDoc localizer (Type.Type ctor tipes)
   in D.hang 4 (D.sep (setup : zipWith (<+>) ("=" : repeat "|") (map ctorDoc ctors)))

aliasToDoc :: L.Localizer -> Name.Name -> Docs.Alias -> D.Doc
aliasToDoc localizer name (Docs.Alias _ tvars tipe) =
  let declaration =
        "type" <+> "alias" <+> D.hsep (map D.fromName (name : tvars)) <+> "="
   in D.hang 4 (D.sep [declaration, typeDoc localizer tipe])

valueToDoc :: L.Localizer -> Name.Name -> Docs.Value -> D.Doc
valueToDoc localizer name (Docs.Value _ tipe) =
  D.hang 4 $ D.sep [D.fromName name <+> ":", typeDoc localizer tipe]

binopToDoc :: L.Localizer -> Name.Name -> Docs.Binop -> D.Doc
binopToDoc localizer name (Docs.Binop _ tipe associativity (Docs.Precedence n)) =
  "(" <> D.fromName name <> ")" <+> ":" <+> typeDoc localizer tipe <> D.black details
  where
    details =
      "    (" <> D.fromName assoc <> "/" <> D.fromInt n <> ")"

    assoc =
      case associativity of
        Docs.Left -> "left"
        Docs.Non -> "non"
        Docs.Right -> "right"

typeDoc :: L.Localizer -> Type.Type -> D.Doc
typeDoc localizer tipe =
  Type.toDoc localizer Type.None tipe
