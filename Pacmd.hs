module Pacmd where

import System.Process (readProcess)
import Text.Parsec (many, notFollowedBy, sepBy, space, manyTill, anyChar, char, parse, many1, spaces, digit, string, Parsec, try)

import Control.Applicative ((<|>), (<*>), (*>), (<*))

endOfLine = char '\n'

data Sink = Sink {sinkindex :: Int, sinkname :: String} deriving (Show)

data Input = Input {inputindex :: Int, inputname :: String, sink :: Int}

list_sinks = pacmd "list-sinks" >>= return . parse parse_sinks "sinks" 

pacmd command = readProcess "pacmd" [command] ""

parse_sinks = garbage >> many parse_sink
	where
	garbage = many garbage_line
	garbage_line = notFollowedBy space >> manyTill anyChar endOfLine

parse_sink = do
	many1 (space <|> char '*')
	string "index:"
	spaces
	num <- many1 digit
	endOfLine
	many $ try $ other_line
	name <- name_line
	many $ try $ other_line
	return $ Sink (read num) name

	where
	name_line = spaces *> string "name:" *> spaces *> char '<' *> manyTill anyChar (char '>') <* manyTill anyChar endOfLine
	other_line = spaces *> notFollowedBy (string "index:" <|> string "name:") *> manyTill anyChar endOfLine