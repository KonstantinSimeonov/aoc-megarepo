{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Prelude
import Data.List
import qualified Data.Text as T

main = do
  input <- readFile "./input"
  let [positions, instructions] = map T.lines $ T.splitOn "\n\n" $ T.pack input
  let stacks = do
      i <- [1, 5..T.length $ head positions]
      pure $ filter ((/=) ' ') $ map (\row -> T.index row i) $ init positions

  let moves = do
      move <- instructions
      let (_:count:_:from:_:to:_) = map (T.unpack) $ T.words move
      pure $ ((read count, read from - 1, read to - 1) :: (Int, Int, Int))

  let part1 = map head $ foldl' (step reverse) stacks moves
  let part2 = map head $ foldl' (step id) stacks moves

  print (part1, part2)

  where
    step moveAlgo stacks (count, from, to) =
      let (crates, left) = splitAt count $ stacks !! from
      in replace from left $ replace to ((moveAlgo crates) ++ (stacks !! to)) stacks

    replace pos new xs = let (left, _:right) = splitAt pos xs in left ++ new:right
