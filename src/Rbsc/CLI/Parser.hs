{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
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
options = do
    optInput <- strArgument
        (metavar "MODEL" <> help helpModelFile)

    optOutput <- optional (strOption
        (metavar "FILE" <> short 'o' <> help helpOutputFile))

    optConstants <- many (strOption
        (metavar "CONST=EXPRESSION" <> short 'c' <> long "const" <> hidden
        <> help helpConstant))

    optMultiActions <- switch
        (short 'm' <> long "multi-actions" <> hidden <> help helpMultiActions)

    optExportSystems <- optional (strOption
        (metavar "FILE" <> long "export-systems" <> hidden
        <> help helpExportSystems))

    optExportDiagrams <- optional (strOption
        (metavar "FILE" <> long "export-diagrams" <> hidden
        <> help helpExportDiagrams ))

    optPrintConstants <- switch
        (long "print-consts" <> hidden <> help helpPrintConsts)

    optRecursionDepth <- RecursionDepth <$> option auto
        ( metavar "INT" <> long "recursion-depth" <> value 100 <> showDefault
        <> hidden <> help helpRecursionDepth)

    optPageWidth <- option auto
        (metavar "INT" <> long "page-width" <> value 120 <> showDefault
        <> hidden <> help helpPageWidth)

    optShowColor <- not <$> switch
        (long "no-color" <> hidden <> help helpNoColor)

    optShowWarnings <- not <$> switch
        (long "no-warn" <> hidden <> help helpNoWarnings)


    optWarnNoSync <- switch
        (long "warn-no-sync" <> hidden <> help helpWarnNoSync)

    optVerbose <- flag NonVerbose Verbose
        (short 'v' <> long "verbose" <> hidden <> help helpVerbose)

    pure Options {..}


helpModelFile      = "The model file (pass - to read from stdin)"
helpOutputFile     = "The output file(s)"
helpConstant       = "Define a constant value"
helpMultiActions   = "Allow multi-actions in generated code"
helpExportSystems  = "Export the full system block"
helpExportDiagrams = "Export the component diagram as Graphviz dot"
helpPrintConsts    = "Print all defined constants"
helpRecursionDepth = "The maximal recursion depth for evaluating expressions"
helpPageWidth      = "The page width used for file output"
helpNoColor        = "Do not use colored output"
helpNoWarnings     = "Suppress warnings"
helpWarnNoSync     = "Warn about unsynchronized actions"
helpVerbose        = "Enable verbose output"


versionInfo :: String
versionInfo = concat
    [ "Version ", showVersion version
    , ", Git revision ", $(gitHash)
    , " (", $(gitCommitDate), ")"
    ]
