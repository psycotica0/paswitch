module Pacmd where

import System.Process (readProcess)
import Text.Parsec (many, notFollowedBy, sepBy, space, manyTill, anyChar, char, parse, many1, (<|>), spaces, digit, string, Parsec, try)

endOfLine = char '\n'

data Sink = Sink {sinkindex :: Int, sinkname :: String}

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
	many $ try $ spaces >> notFollowedBy (string "index:") >> manyTill anyChar endOfLine
	return num
