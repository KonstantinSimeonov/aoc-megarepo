import Prelude
import System.IO
import Data.List.Split
import Data.List
import Control.Monad
import qualified Data.Set as S

step size img algorithm stepIndex = S.fromList $ do
  y <- [0..size]
  x <- [0..size]
  let bin = int $ do
      ny <- [y - 1..y + 1]
      nx <- [x - 1..x + 1]
      let isOutside = ny <= 0 || nx <= 0 || ny >= size - 1 || nx >= size -1
          bit = if S.member (ny, nx) img then 1 else 0
      pure $ if isOutside then def else bit
  guard $ S.member bin algorithm
  pure (y + 1, x + 1)
  where
    def = if S.member 0 algorithm then stepIndex `mod` 2 else 0

    int :: [Int] -> Int
    int = foldl' (\s x -> s * 2 + x) 0

main = do
  [algorithmStr, imgStr] <- splitOn "\n\n" <$> readFile "./input1"
  let algorithm = S.fromList $ do
          (x, i) <- zip algorithmStr [0..]
          guard $ x == '#'
          pure i

      (Just w) = findIndex ((==) '\n') imgStr

      img = S.fromList $ do
          let grid = init $ splitOn "\n" imgStr
              indexed = zip [1..] $ map (zip [1..]) grid
          (y, row) <- indexed
          (x, bit) <- row
          guard $ bit == '#'
          pure (y, x)

      result = scanl' (\im (size, i) -> step size im algorithm i) img $ zip [w + 2, w + 4..] [0..]

  putStrLn $ "part 1: " ++ (show $ S.size $ result !! 2)
  putStrLn $ "part 2: " ++ (show $ S.size $ result !! 50)
