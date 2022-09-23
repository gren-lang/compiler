{-# LANGUAGE OverloadedStrings #-}

module Package.Outdated
  ( Args (..),
    Flags (..),
    run,
  )
where

import Deps.Solver qualified as Solver
import Directories qualified as Dirs
import Gren.Outline qualified as Outline
import Reporting qualified
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task

-- RUN

data Args
  = NoArgs

data Flags = Flags
  { _skipPrompts :: Bool
  , _unsafe :: Bool
  }

run :: Args -> Flags -> IO ()
run _ (Flags _skipPrompts _unafe) =
  Reporting.attempt Exit.installToReport $
    do
      maybeRoot <- Dirs.findRoot
      case maybeRoot of
        Nothing ->
          return (Left Exit.InstallNoOutline)
        Just root ->
          Task.run $
            do
              env <- Task.io Solver.initEnv
              oldOutline <- Task.eio Exit.InstallBadOutline $ Outline.read root
              installDependencies env oldOutline

-- LIST OUTDATED

type Task = Task.Task Exit.Install

installDependencies :: Solver.Env -> Outline.Outline -> Task ()
installDependencies (Solver.Env _) _ =
    return ()

