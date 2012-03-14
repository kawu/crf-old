{-# LANGUAGE FlexibleInstances #-}

module Data.CRF.InOut
( WordR (..)
, WordRM (..)
, SentR
, SentRM
, readUnmarked
, readMarked
) where

import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text as TS
import Data.List (groupBy, intercalate)
import Data.Function (on)
import Control.Monad (when, (>=>))
import Control.Applicative ((<$>), (<*>))

-- Zakładamy, że napisy reprezentujące obserwacje/etykiety
-- pozbawione są białych znaków (np. spacji) oraz znaku '|'.

-- | Word with restrictions.  When the set of restrictions is empty,
-- any label (from the set of all labels) can be chosen. 
data WordR t = WordR [t] [t]
-- | Word with restrictions and choices.
data WordRM t = WordRM [t] [t] [(t, Double)]

-- | Custom show class, since show for T.Text doesn't work as expected.
class MShow t where
    mshow :: t -> String

instance MShow Int where
    mshow = show

instance MShow TS.Text where
    mshow = TS.unpack

instance MShow t => Show (WordR t) where
    show (WordR os rs) =
        intercalate " " (map mshow os) ++
        " | " ++
        intercalate " " (map mshow rs)

instance MShow t => Show (WordRM t) where
    show (WordRM os rs ys) =
        intercalate " " (map mshow os) ++
        " | " ++
        intercalate " " (map mshow rs) ++
        " | " ++
        intercalate " " (map showY ys)
      where
        showY (y, prob) = mshow y ++ "#" ++ show prob

-- | Line + position (number) in input stream. 
data LineNum = LineNum Int T.Text
lineNum (LineNum n _) = n
lineText (LineNum _ t) = t

type SentR t = [WordR t]
type SentRM t = [WordRM t]

-- | Parser of type (Parser t) returns either string error message or t object.
type Parser t = T.Text -> Either String t

-- | Parsing rules for basic data types.
class ReadT t where
    readT :: Parser t

instance ReadT Double where
    readT = T.double >=> return . fst

instance ReadT Int where
    readT = T.decimal >=> return . fst

instance ReadT T.Text where
    readT = return . id

instance ReadT TS.Text where
    readT = return . T.toStrict

instance ReadT t => ReadT (t, Double) where
    readT input =
      case length parts of 
        1 -> (,) <$> readT first <*> return 1.0
        2 -> (,) <$> readT first <*> readT second
        _ -> Left $ "Incorrect number of #"
      where
        parts = T.splitOn hashSep input
        first = head parts
        second = head $ tail parts

pipeSep = T.singleton '|'
spaceSep = T.singleton ' '
hashSep = T.singleton '#'

groupSents :: [LineNum] -> [[LineNum]]
groupSents = filter (not . T.null . lineText . head)
           . groupBy ((==) `on` T.null . lineText)

-- | Adorn potential error message with line number.
addLineNum :: Int -> Either String t -> Either String t
addLineNum n (Left e) = Left $ "(Line " ++ show n ++ ") " ++ e
addLineNum _ x = x

-- readSentR :: ReadT t => [LineNum] -> Either String (SentR t)
-- readSentR = mapM $ \x ->
--     addLineNum (lineNum x) $ readWordR (lineText x)
-- 
-- readSentRM :: ReadT t => [LineNum] -> Either String (SentRM t)
-- readSentRM = mapM $ \x ->
--     addLineNum (lineNum x) $ readWordRM (lineText x)

-- | TODO: dodawanie informacji o linii -- wykorzystac funkcje either.
readSent :: Parser a -> [LineNum] -> Either String [a]
readSent p = mapM $ \x ->
    addLineNum (lineNum x) $ p (lineText x)

readWordR :: ReadT t => Parser (WordR t)
readWordR line = do
    when (length parts /= 2) $ Left "Incorrent number of sections"
    obvs <- mapM readT obsPart 
    resr <- mapM readT rPart
    return $ WordR obvs resr
  where
    parts = map (split . T.strip) $ T.splitOn pipeSep line
    split = filter (not . T.null) . T.splitOn spaceSep
    [obsPart, rPart] = parts

readWordRM :: ReadT t => Parser (WordRM t)
readWordRM line = do
    when (length parts /= 3) $ Left "Incorrent number of sections"
    obvs <- mapM readT obsPart 
    resr <- mapM readT rPart
    choices <- mapM readT cPart
    return $ WordRM obvs resr choices
  where
    parts = map (split . T.strip) $ T.splitOn pipeSep line
    split = filter (not . T.null) . T.splitOn spaceSep
    [obsPart, rPart, cPart] = parts

readData :: Parser a -> T.Text -> [Either String [a]]
readData p input =
    let lines = T.lines input
        lineNums = [LineNum k line | (k, line) <- zip [1..] lines]
    in  map (readSent p) (groupSents lineNums)

readUnmarked :: ReadT t => T.Text -> [Either String (SentR t)]
readUnmarked = readData readWordR

readMarked :: ReadT t => T.Text -> [Either String (SentRM t)]
readMarked = readData readWordRM
