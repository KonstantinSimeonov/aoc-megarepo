{-# LANGUAGE BangPatterns #-}

import Prelude
import System.IO
import Data.List.Split
import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

rotY (!x, !y, !z) = (-z, y, x)
rotX (!x, !y, !z) = (x, -z, y)

data Rot = RX | RY deriving (Show, Eq)

rots = map snd $ M.toList stuff
  where
    stuff = foldl' (\m rs -> let fn = compose rs in M.insert (fn (1, 2, 3)) fn m) M.empty allRots
    allRots = tail $ perm [RX, RX, RX] [RY, RY, RY] []
    perm xs ys acc =
      let xr = if null xs then [] else (perm (tail xs) ys ((head xs):acc))
          yr = if null ys then [] else (perm xs (tail ys) ((head ys):acc))
      in acc:(xr ++ yr)

    compose [] = id
    compose (RX:rs) = rotX . (compose rs)
    compose (RY:rs) = rotY . (compose rs)

brr xs = do
  r <- rots
  pure $ map r xs

(!a, !b, !c) <-> (!d, !e, !f) = (a - d, b - e, c - f)
(!a, !b, !c) <+> (!d, !e, !f) = (a + d, b + e, c + f)

overlap scanner0 scanner1 = case catMaybes $ concatMap id $ matches of
  [] -> Nothing
  (x:_) -> Just x
  where
    s0 = S.fromList scanner0
    matches = do
        rotated1 <- brr scanner1
        let possibleTranslations = (<->) <$> scanner0 <*> rotated1
        let translationMatches = do
            t <- possibleTranslations
            let translated1 = map ((<+>) t) rotated1
            let (!matching, !rest) = partition (\p -> S.member p s0) translated1
            pure $ if length matching >= 12
                then Just (S.union s0 $ S.fromList rest, t)
                else Nothing
        pure translationMatches

cartograph coords =
  if null left
  then (centers, mapped)
  else let (centers', allMapped) = cartograph (mapped:left)
       in (centers ++ centers', allMapped)
  where
    (mapped, (centers, left)) =
      foldl' (\(!scanner0, (!centers, !notMatched)) !scanner1 ->
                case overlap scanner0 scanner1 of
                  Just (!unified, !center) -> (S.toList unified, (center:centers, notMatched))
                  Nothing -> (scanner0, (centers, scanner1:notMatched))
              ) (head coords, ([], [])) $ tail coords

manhattan centers = maximum $ do
  (x, y, z) <- xs
  (a, b, c) <- xs
  pure $ foldl1 (+) $ map abs [x - a, y - b, z - c]
  where
    xs = (0, 0, 0):(nub centers)

readInput = do
  input <- init <$> readFile "./input2"
  let scanners = do
      s <- splitOn "\n\n" input
      pure $ tail $ do
        pts <- splitOn "\n" s :: [String]
        pure $ pts

  let scannerCoords = do
      s <- scanners
      pure $ do
        p <- s
        let [x, y, z] = map read $ splitOn "," p :: [Int]
        pure (x, y, z)

  pure scannerCoords

main = do
  scanners <- readInput
  let (centers, scans) = cartograph scanners
  putStrLn $ intercalate "" $ map (\x -> show x ++ "\n") $ scans
  print $ length $ scans
  print centers
  print $ manhattan centers
