import Prelude
import System.IO
import Data.List.Split
import Data.List
import qualified Data.Map as M

main = do
  [templateStr, polysStr] <- splitOn "\n\n" <$> readFile "./input"
  let polys = M.fromList $ do {
    [from, to] <- map (splitOn " -> ") $ splitOn "\n" polysStr;
    pure (from, head to);
  }
  let template = foldl' (\m p -> M.insertWith (+) p 1 m) M.empty $ zipWith (\x y -> [x, y]) templateStr (tail templateStr)

  let simulations = map countFreqs $ scanl' (\x _ -> step x polys) template [0..]
  let frequences10 = simulations !! 10
  print ("part1", (maximum frequences10) - (minimum frequences10))
  let frequences40 = simulations !! 40
  print ("part2", (maximum frequences40) - (minimum frequences40))

  where
    step :: M.Map String Int -> M.Map String Char -> M.Map String Int
    step pairs toInsert = M.foldlWithKey step' M.empty pairs
      where
        step' m [a, b] k = let e = toInsert M.! [a, b]
                           in M.insertWith (+) [a, e] k . M.insertWith (+) [e, b] k $ m

    countFreqs =
      map (\(_, f) -> (f + 1) `div` 2)
      . M.toList
      . M.foldlWithKey (\m [a, b] f -> M.insertWith (+) a f $ M.insertWith (+) b f m) M.empty
