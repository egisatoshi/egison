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
import qualified Text.Parsec as P

data EgisonOpts = EgisonOpts {
    optExecFile         :: Maybe (String, [String]),
    optShowVersion      :: Bool,
    optEvalString       :: Maybe String,
    optExecuteString    :: Maybe String,
    optFieldInfo        :: [(String, String)],
    optLoadLibs         :: [String],
    optLoadFiles        :: [String],
    optSubstituteString :: Maybe String,
    optMapTsvInput      :: Maybe String,
    optFilterTsvInput   :: Maybe String,
    optTsvOutput        :: Bool,
    optNoIO             :: Bool,
    optShowBanner       :: Bool,
    optTestOnly         :: Bool,
    optPrompt           :: String,
    optMathExpr         :: Maybe String,
    optSExpr            :: Bool,
    optMathNormalize    :: Bool
    }

defaultOption :: EgisonOpts
defaultOption = EgisonOpts Nothing False Nothing Nothing [] [] [] Nothing Nothing Nothing False False True False "> " Nothing False True

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
