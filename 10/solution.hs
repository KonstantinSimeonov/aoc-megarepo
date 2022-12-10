import System.IO
import Data.List

main = do
  input <- lines <$> readFile "./input1"
  let (_, _, sum) = foldl' step (1, 0, 0) input
  print $ sum

  where
    step :: (Int, Int, Int) -> String -> (Int, Int, Int)
    step state "noop" = cycle state
    step state add =
      let [_, value] = words add
          (x, i, sum) = cycle $ cycle state
      in (x + read value, i, sum)

    cycle :: (Int, Int, Int) -> (Int, Int, Int)
    cycle (x, i, sum) =
      let newi = i + 1
          newSum = sum + if newi `mod` 40 == 20 then (x * newi) else 0
      in (x, newi, newSum)
