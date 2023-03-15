import System.IO
import Data.List

main = do
  input <- lines <$> readFile "./input"
  let (_, _, sum, screen) = foldl' step (1, 0, 0, "") input
  print sum
  putStrLn $ intercalate "\n" $ chunks 40 $ reverse screen

  where
    step state "noop" = cycle state
    step state add =
      let (x, i, sum, screen) = cycle $ cycle state
      in (x + (read $ drop 5 $ add), i, sum, screen)

    cycle (x, i, sum, screen) =
      let newi = i + 1
          newSum = sum + if newi `mod` 40 == 20 then (x * newi) else 0
          spriteDist = abs $ x - (i `mod` 40)
          newScreen = (if spriteDist <= 1 then '#' else ' '):screen
      in (x, newi, newSum, newScreen)

    chunks n [] = []
    chunks n xs = let (chunk, rest) = splitAt n xs in chunk:chunks n rest
