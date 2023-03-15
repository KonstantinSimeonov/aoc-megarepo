import Prelude
import Data.List

rolls = [1..100] ++ rolls

play (_:b:_:_:e:_:xs) p1 p2
  | (snd p1) >= 1000 || (snd p2) >= 1000 = [(p1, p2)]
  | otherwise = (p1, p2):play xs (step p1 (3 * b)) (step p2 (3 * e))

step :: (Int, Int) -> Int -> (Int, Int)
step (p, s) x = let x' = x `mod` 10
                    p' = p + x'
                    p'' = if p' > 10 then p' `mod` 10 else p'
                in (p'', s + p'')

part1 =
  let game = tail $ play rolls (10, 0) (4, 0)
      (s1, s2):(s1', s2'):_ = map (\(p1, p2) -> (snd p1, snd p2)) $ reverse game
      l = 6 * length game
      (rc, loser) = if s1 >= 1000 then (l - 3, s2') else (l, s1)
  in rc * loser

quantomRolls = map (\g -> (length g, head g)) $ group $ sort $ allRolls
  where
    allRolls = do
      d1 <- [1, 2, 3]
      d1' <- [1, 2, 3]
      d1'' <- [1, 2, 3]
      pure (d1 + d1' + d1'')

main = do
  print part1
  print $ quantomRolls
