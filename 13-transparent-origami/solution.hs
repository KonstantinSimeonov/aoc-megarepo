{-# LANGUAGE OverloadedStrings #-}

import Prelude
import System.IO (readFile)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Char (ord)
import qualified Data.List as L

readInt :: T.Text -> Int
readInt = T.foldl' (\acc x -> acc * 10 + (ord x - 48)) 0

foldPoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
foldPoint (foldx, foldy) (x, y) = (x', y')
  where
    x' = if x < foldx then x else 2 * foldx - x
    y' = if y < foldy then y else 2 * foldy - y

pretty :: S.Set (Int, Int) -> String
pretty xs = L.intercalate "\n" points
  where
    points = [ [if S.member (x, y) xs then '█' else ' ' | x <- [0..cols] ] | y <- [0..rows] ]
    cols = maximum $ map fst ps
    rows = maximum $ map snd ps
    ps = S.toList xs

parsePoints :: [T.Text] -> S.Set (Int, Int)
parsePoints lines = S.fromList $ do
  line <- lines
  let [x, y] = T.splitOn "," line
  pure (readInt x, readInt y)

parseFolds :: [T.Text] -> [(Int, Int)]
parseFolds foldsStr = do
  fold <- foldsStr
  let [axis, value] = T.splitOn "=" fold
  pure $ case axis of
    "x" -> (readInt value, maxBound)
    "y" -> (maxBound, readInt value)

main = do
  input <- T.stripEnd . (T.replace "fold along " "") . T.pack <$> readFile "./input1"
  let [coordsStr, foldsStr] = map (T.splitOn "\n") $ T.splitOn "\n\n" input
  let points = parsePoints coordsStr
  let folds = parseFolds foldsStr
  let firstFold = S.map (foldPoint $ head folds) points
  putStrLn $ "part 1: " ++ show (S.size firstFold)

  let completelyFolded = L.foldl' (\s f -> S.map (foldPoint f) s) points folds
  putStrLn "part 2:"
  putStrLn $ pretty completelyFolded
