-- | Representation of command line options.
module Rbsc.CLI.Options
    ( Options(..)
    , Verbosity(..)
    ) where


import Data.Text (Text)


import Rbsc.Config


-- | The command line options.
data Options = Options
    { -- | The input file.
      optInput          :: FilePath

      -- | The output path. If multiple systems are generated, an index is
      -- added between the file name and the file extension.
    , optOutput         :: Maybe FilePath

      -- | A list of constant definitions.
    , optConstants      :: [Text]

      -- | Enable/disable the generation of multi-action code.
    , optMultiActions   :: !Bool

      -- | Export the full system block for each generated system instance.
    , optExportSystems  :: Maybe FilePath

      -- | Export the component instance diagrams.
    , optExportDiagrams :: Maybe FilePath

      -- | Print all constant values.
    , optPrintConstants :: !Bool

      -- | The maximal recursion depth for evaluating expressions.
    , optRecursionDepth :: !RecursionDepth

      -- | The page width used for file output.
    , optPageWidth      :: !Int

      -- | Enable/disable colored output.
    , optShowColor      :: !Bool

      -- | Enable/disable warnings.
    , optShowWarnings   :: !Bool

      -- | Enable/disable warnings about unsynchronized actions.
    , optWarnNoSync     :: !Bool

      -- | Enable/disable verbose output
    , optVerbose        :: !Verbosity
    }


data Verbosity = NonVerbose | Verbose
