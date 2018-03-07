module Brainscrambler.Parser
       ( parse
       ) where

import           Universum

import           Text.Megaparsec            (Parsec, eof, runParser)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Brainscrambler.AST         (Brainscrambler)
import qualified Brainscrambler.AST         as AST

type Parser = Parsec Void String

token :: Parser (Brainscrambler ())
token = AST.increment         <$ char '+'
    <|> AST.decrement         <$ char '-'
    <|> AST.pushZero          <$ char '*'
    <|> AST.pop               <$ char '^'
    <|> AST.output            <$ char '.'
    <|> AST.rotate            <$ char '#'
    <|> AST.moveHeadToLeft    <$ char '<'
    <|> AST.moveHeadToRight   <$ char '>'
    <|> AST.cycleStart        <$ char '['
    <|> AST.cycleEnd          <$ char ']'
    <|> AST.input             <$> (char ',' *> decimal)

program :: Parser (Brainscrambler ())
program = liftA2 (>>) token program <|> eof $> pure ()

parse :: String -> Maybe (Brainscrambler ())
parse = rightToMaybe . runParser program ""
