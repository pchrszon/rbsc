{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}


module Rbsc.CLI.Parser
    ( parseOptions
    ) where


import Data.Version

import Development.GitRev

import Options.Applicative


import Rbsc.Config

import Rbsc.CLI.Options


import Paths_roles (version)


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (helper <*> versionOpt <*> options)
         ( fullDesc
        <> header "rbsc - role-based system compiler" )

    versionOpt =
        infoOption versionInfo
            ( long "version"
           <> hidden
           <> help "Show version information")


options :: Parser Options
options = Options
    <$> optional (strArgument
        ( metavar "MODEL"
       <> help helpModelFile ))
    <*> optional (strOption
        ( metavar "FILE"
       <> short 'o'
       <> help helpOutputFile ))
    <*> optional (strOption
        ( metavar "FILE"
       <> long "export-systems"
       <> help helpExportSystems ))
    <*> optional (strOption
        ( metavar "FILE"
       <> long "export-diagrams"
       <> help helpExportDiagrams ))
    <*> (RecursionDepth <$> option auto
        ( metavar "INT"
       <> long "recursion-depth"
       <> value 100
       <> showDefault
       <> help helpRecursionDepth ))
    <*> flag True False
        ( long "no-color"
       <> help helpNoColor )
    <*> flag True False
        ( long "no-warn"
       <> help helpNoWarnings )
    <*> flag NonVerbose Verbose
        ( short 'v'
       <> long "verbose"
       <> help helpVerbose )


helpModelFile      = "The model file"
helpOutputFile     = "The output file(s)"
helpExportSystems  = "Export the full system block"
helpExportDiagrams = "Export the component diagram as Graphviz dot"
helpRecursionDepth = "The maximal recursion depth for evaluating expressions"
helpNoColor        = "Do not use colored output"
helpNoWarnings     = "Suppress warnings"
helpVerbose        = "Enable verbose output"


versionInfo :: String
versionInfo = concat
    [ "Version ", showVersion version
    , ", Git revision ", $(gitHash)
    , " (", $(gitCommitDate), ")"
    ]