module Pacmd where

import System.Process (readProcess)
import Text.Parsec (many, notFollowedBy, sepBy, space, manyTill, anyChar, char, parse, many1, spaces, digit, string, Parsec, try, lookAhead, eof)

import Control.Applicative ((<|>), (<*>), (*>), (<*), pure)
import Control.Monad (void)

endOfLine = char '\n'

data Sink = Sink {sinkindex :: Int, sinkname :: String} deriving (Show)

data Input = Input {inputindex :: Int, inputname :: String, sink :: Int} deriving (Show)

list_sinks = pacmd "list-sinks" >>= return . parse parse_sinks "sinks" 

pacmd command = readProcess "pacmd" [command] ""

garbage_line = notFollowedBy space >> manyTill anyChar (void eof <|> void endOfLine)
garbage = many garbage_line

parse_sinks = garbage >> many parse_sink

line = manyTill anyChar endOfLine

indexLine = many1 (space <|> char '*') *> string "index:" *> spaces *>
	many1 digit <* endOfLine

colonLine key valueParser = spaces *> string key *> spaces *> valueParser <* manyTill anyChar endOfLine

data SinkLine = Name String | Other
sinkLine = try nameLine <|> otherLine
	where
	nameLine = fmap Name $ colonLine "name:" $ char '<' *> manyTill anyChar (char '>')
	otherLine = pure Other <* line

endOfItem = (void garbage_line) <|> (void $ lookAhead $ try indexLine)

getName = foldr func Nothing
	where
	func (Name name) = const $ Just name
	func _ = id

parse_sink = do
	num <- indexLine
	name <- fmap getName $ manyTill sinkLine endOfItem
	return $ fmap (Sink (read num)) name

data InputLine = Client String | InputSink Int | InputOther
inputLine = try clientLine <|> try sinkLine <|> otherLine
	where
	clientLine = fmap Client $ colonLine "client:" $ many1 digit *> spaces *> char '<' *> manyTill anyChar (char '>')
	sinkLine = fmap (InputSink . read) $ colonLine "sink:" $ many1 digit
	otherLine = pure InputOther <* line

getInputData = consolidate . foldr func (Nothing, Nothing)
	where
	func (Client name) (_, s) = (Just name, s)
	func (InputSink num) (n, _) = (n, Just num)
	func _ a = a
	consolidate (Just x, Just y) = Just (x, y)
	consolidate _ = Nothing

list_inputs = pacmd "list-sink-inputs" >>= return . parse parse_inputs "inputs"

parse_inputs = garbage *> many parse_input

parse_input = do
	num <- indexLine
	stuff <- fmap getInputData $ manyTill inputLine endOfItem
	return $ fmap (uncurry $ Input (read num)) stuff
