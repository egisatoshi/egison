{- |
Module      : Language.Egison.CmdOptions
Licence     : MIT

This module provides command line options of Egison interpreter.
-}

module Language.Egison.CmdOptions
  ( EgisonOpts (..)
  , defaultOption
  , cmdParser
  ) where

import           Data.Functor        (($>))
import           Data.List           (intercalate)
import           Data.Maybe          (maybeToList)
import           Options.Applicative
import qualified Text.Parsec         as P

-- | Command-line options for the Egison interpreter.
--   See https://egison.readthedocs.io/en/latest/reference/command-line-options.html
--   for the detailed description of some of the fields.
data EgisonOpts = EgisonOpts {
    -- | Execute the main function of the given file with command-line options.
    optExecFile         :: Maybe (String, [String]),
    -- | When True, show version number and exit.
    optShowVersion      :: Bool,
    -- | Evaluate the input string, which is an expression, and exit.
    optEvalString       :: Maybe String,
    -- | Execute the input string, which is a toplevel expression, and exit.
    optExecuteString    :: Maybe String,
    -- | Specify how to interpret the TSV input.
    optFieldInfo        :: [(String, String)],
    -- | Names of libraries to load.
    optLoadLibs         :: [String],
    -- | Names of files to load.
    optLoadFiles        :: [String],
    -- | For the -s / --substitute option.
    optSubstituteString :: Maybe String,
    -- | For the -m / --map option.
    optMapTsvInput      :: Maybe String,
    -- | For the -f / --filter option.
    optFilterTsvInput   :: Maybe String,
    -- | Output results in TSV format. Default is False.
    optTsvOutput        :: Bool,
    -- | Disable loading from external files\/libraries and I\/O primitive
    --   functions. Default is False.
    optNoIO             :: Bool,
    -- | Show the interpreter banner. Default is True.
    optShowBanner       :: Bool,
    -- | Do not execute the main function and only evaluate the top-level
    --   expressions in the file. Default is False.
    optTestOnly         :: Bool,
    -- | Prompt of the interpreter. Default is "> ".
    optPrompt           :: String,
    -- | Output results in markup format. Available options are latex,
    --   asciimath, mathematica and maxima
    optMathExpr         :: Maybe String,
    -- | Use the old parser. Default is False.
    optSExpr            :: Bool,
    -- | Enable simplification of math expressions by term rewriting.
    --   Default is True.
    optMathNormalize    :: Bool
    }

-- | Default settings for EgisonOpts.
defaultOption :: EgisonOpts
defaultOption = EgisonOpts Nothing False Nothing Nothing [] [] [] Nothing Nothing Nothing False False True False "> " Nothing False True

-- | Command-line argument parser. Used in the interpreter.
cmdParser :: ParserInfo EgisonOpts
cmdParser = info (helper <*> cmdArgParser)
          $ fullDesc
          <> header "The Egison Programming Language"

cmdArgParser :: Parser EgisonOpts
cmdArgParser = EgisonOpts
            <$> optional ((,) <$> strArgument (metavar "FILE") <*> many (strArgument (metavar "ARGS")))
            <*> switch
                  (short 'v'
                  <> long "version"
                  <> help "Show version number")
            <*> optional (strOption
                  (short 'e'
                  <> long "eval"
                  <> metavar "EXPR"
                  <> help "Evaluate the argument string"))
            <*> optional (strOption
                  (short 'c'
                  <> long "command"
                  <> metavar "EXPR"
                  <> help "Execute the argument string"))
            <*> many (option readFieldOption
                  (short 'F'
                  <> long "field"
                  <> metavar "FIELD"
                  <> help "Field information"))
            <*> many (strOption
                  (short 'L'
                  <> long "load-library"
                  <> metavar "FILE"
                  <> help "Load library"))
            <*> many (strOption
                  (short 'l'
                  <> long "load-file"
                  <> metavar "FILE"
                  <> help "Load file"))
            <*> optional (strOption
                  (short 's'
                  <> long "substitute"
                  <> metavar "EXPR"
                  <> help "Operate input in tsv format as infinite stream"))
            <*> optional (strOption
                  (short 'm'
                  <> long "map"
                  <> metavar "EXPR"
                  <> help "Operate input in tsv format line by line"))
            <*> optional (strOption
                  (short 'f'
                  <> long "filter"
                  <> metavar "EXPR"
                  <> help "Filter input in tsv format line by line"))
            <*> switch
                  (short 'T'
                  <> long "tsv"
                  <> help "Output in tsv format")
            <*> switch
                  (long "no-io"
                  <> help "Prohibit all io primitives")
            <*> flag True False
                  (long "no-banner"
                  <> help "Do not display banner")
            <*> switch
                  (short 't'
                  <> long "test"
                  <> help "Execute only test expressions")
            <*> strOption
                  (short 'p'
                  <> long "prompt"
                  <> metavar "STRING"
                  <> value "> "
                  <> help "Set prompt string")
            <*> optional (strOption
                  (short 'M'
                  <> long "math"
                  <> metavar "(asciimath|latex|mathematica|maxima)"
                  <> help "Output in AsciiMath, Latex, Mathematica, or Maxima format"))
            <*> flag False True
                  (short 'S'
                  <> long "sexpr-syntax"
                  <> help "Use s-expression syntax")
            <*> flag True False
                  (long "no-normalize"
                  <> help "Turn off normalization of math expressions")

readFieldOption :: ReadM (String, String)
readFieldOption = eitherReader $ \str ->
  case P.parse parseFieldOption "(argument)" str of
    Left err -> Left $ show err
    Right ok -> Right ok

parseFieldOption :: P.Parsec String () (String, String)
parseFieldOption = do
  s <- P.many1 P.digit
  e <- P.optionMaybe (P.char ',' >> P.many1 P.digit)
  let se = s : maybeToList e
  (rs, rc)
    <-  P.try (P.string "sc") $> (se, se)
    <|> P.try (P.string "cs") $> (se, se)
    <|> P.try (P.string "s" ) $> (se, [])
    <|> P.try (P.string "c" ) $> ([], se)
  P.eof
  let f x = "[" ++ intercalate ", " x ++ "]"
  return (f rs, f rc)
