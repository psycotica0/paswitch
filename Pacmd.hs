module Pacmd where

import System.Process (readProcess)
import Text.Parsec (many, notFollowedBy, sepBy, space, manyTill, anyChar, char, parse, many1, spaces, digit, string, Parsec, try, lookAhead, eof)

import Data.Maybe (catMaybes)
import Control.Applicative ((<|>), (<*>), (*>), (<*), pure)
import Control.Monad (void)
import Data.Foldable (foldMap)
import Data.Monoid (First(..), getFirst)

endOfLine = char '\n'
garbage_line = notFollowedBy space >> manyTill anyChar (void eof <|> void endOfLine)
garbage = many garbage_line
line = manyTill anyChar endOfLine
endOfItem = (void garbage_line) <|> (void $ lookAhead $ try indexLine)

colonLine key valueParser = spaces *> string key *> spaces *> valueParser <* manyTill anyChar endOfLine
indexLine = fmap read $ many1 (space <|> char '*') *> string "index:" *> spaces *>
	many1 digit <* endOfLine

pacmd command = readProcess "pacmd" [command] ""

data Sink = Sink {sinkindex :: Int, sinkname :: String} deriving (Show)

list_sinks = pacmd "list-sinks" >>= return . fmap catMaybes . parse parse_sinks "sinks"

parse_sinks = garbage >> many parse_sink

sinkLine = try nameLine <|> otherLine
	where
	nameLine = fmap Just $ colonLine "name:" $ char '<' *> manyTill anyChar (char '>')
	otherLine = pure Nothing <* line

getName = getFirst . foldMap First

parse_sink = do
	num <- indexLine
	name <- fmap getName $ manyTill sinkLine endOfItem
	return $ fmap (Sink num) name

data Input = Input {inputindex :: Int, inputname :: String, sink :: Int} deriving (Show)

data InputLine = Client String | InputSink Int | InputOther
inputLine = try clientLine <|> try sinkLine <|> otherLine
	where
	clientLine = fmap Client $ colonLine "client:" $ many1 digit *> spaces *> char '<' *> manyTill anyChar (char '>')
	sinkLine = fmap (InputSink . read) $ colonLine "sink:" $ many1 digit
	otherLine = pure InputOther <* line

getInputData = consolidate . unFirst . foldMap (first . func)
	where
	first (x, y) = (First x, First y)
	unFirst (First x, First y) = (x, y)
	func (Client name) = (Just name, Nothing)
	func (InputSink num) = (Nothing, Just num)
	func _ = (Nothing, Nothing)
	consolidate (Just x, Just y) = Just (x, y)
	consolidate _ = Nothing

list_inputs = pacmd "list-sink-inputs" >>= return . fmap catMaybes . parse parse_inputs "inputs"

parse_inputs = garbage *> many parse_input

parse_input = do
	num <- indexLine
	stuff <- fmap getInputData $ manyTill inputLine endOfItem
	return $ fmap (uncurry $ Input num) stuff
