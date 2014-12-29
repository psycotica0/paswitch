module Pacmd (
	Sink(..), Input(..),
	list_sinks, list_inputs,
	set_default_sink
) where

import System.Process (readProcess)
import Text.Parsec (many, notFollowedBy, sepBy, space, manyTill, anyChar, char, parse, many1, spaces, digit, string, Parsec, try, lookAhead, eof)

import Data.Maybe (catMaybes)
import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*), pure)
import Control.Monad (void)
import Data.Foldable (foldMap)
import Data.Monoid (First(..), getFirst)

endOfLine = char '\n'
garbage_line = notFollowedBy space >> manyTill anyChar (void eof <|> void endOfLine)
garbage = many garbage_line
line = manyTill anyChar endOfLine
endOfItem = (void garbage_line) <|> (void $ lookAhead $ try indexLine)

colonLine key valueParser = spaces *> string key *> spaces *> valueParser <* manyTill anyChar endOfLine

indexLine = (,) <$> spacesDefault <*> num
	where
	spacesDefault = fmap (any (== '*')) $ many1 $ space <|> char '*'
	num = fmap read $ string "index:" *> spaces *> many1 digit <* endOfLine

pacmd command = readProcess "pacmd" [command] ""

data Sink = Sink {sinkindex :: Int, sinkdefault :: Bool, sinkname :: String} deriving (Show)

list_sinks = pacmd "list-sinks" >>= return . fmap catMaybes . parse parse_sinks "sinks"

parse_sinks = garbage >> many parse_sink

sinkLine = try nameLine <|> otherLine
	where
	nameLine = fmap Just $ colonLine "name:" $ char '<' *> manyTill anyChar (char '>')
	otherLine = pure Nothing <* line

getName = getFirst . foldMap First

parse_sink = do
	(def, num) <- indexLine
	name <- fmap getName $ manyTill sinkLine endOfItem
	return $ fmap (Sink num def) name

set_default_sink (Sink {sinkindex = v}) = pacmd $ "set-default-sink " ++ show v

data Input = Input {inputindex :: Int, inputname :: String, sink :: Int} deriving (Show)

inputLine = try clientLine <|> try sinkLine <|> (fmap e line)
	where
	c v = (Just v, Nothing)
	s v = (Nothing, Just v)
	e = const (Nothing, Nothing)
	clientLine = fmap c $ colonLine "client:" $ many1 digit *> spaces *> char '<' *> manyTill anyChar (char '>')
	sinkLine = fmap s $ fmap (read) $ colonLine "sink:" $ many1 digit

first2 (a, b) = (First a, First b)
getFirst2 (First (Just a), First (Just b)) = Just (a, b)
getFirst2 _ = Nothing

getInput = getFirst2 . foldMap first2

list_inputs = pacmd "list-sink-inputs" >>= return . fmap catMaybes . parse parse_inputs "inputs"

parse_inputs = garbage *> many parse_input

parse_input = do
	num <- fmap snd indexLine
	stuff <- fmap getInput $ manyTill inputLine endOfItem
	return $ fmap (uncurry $ Input num) stuff
