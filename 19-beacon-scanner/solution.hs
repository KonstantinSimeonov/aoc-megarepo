import Prelude
import System.IO
import Data.List.Split
import Data.Maybe
import Data.List
import qualified Data.Set as S

type P3 = (Int, Int, Int)

(<->) :: P3 -> P3 -> P3
(a, b, c) <-> (d, e, f) = (a - d, b - e, c - f)

(<+>) :: P3 -> P3 -> P3
(a, b, c) <+> (d, e, f) = (a + d, b + e, c + f)

(<**>) :: P3 -> P3 -> P3
(a, b, c) <**> (d, e, f) = (a * d, b * e, c * f)

brr xs = [ [ x' <**> (x, y, z) | x' <- xs ] | x <- [1, -1], y <- [1, -1], z <- [1, -1] ]

hopt [] = Nothing
hopt (x:_) = Just x

overlap s0 s1 = hopt $ catMaybes $ concatMap id $ test
  where
    x0 = S.fromList s0
    test = do
        a1 <- brr s1
        let vs = (<->) <$> s0 <*> a1
        let stuff = do
            v <- vs
            let ts = map ((<+>) v) a1
            let (c, nc) = partition (\t -> S.member t x0) ts
            pure $ if length c >= 12
                then Just (S.union x0 $ S.fromList nc, (v, [0]))
                else Nothing
        pure stuff

main = do
  input <- init <$> readFile "./input"
  let sc = do
      s <- splitOn "\n\n" input
      pure $ tail $ do
        pts <- splitOn "\n" s :: [String]
        pure $ pts
  let coords = do
      s <- sc
      pure $ do
        p <- s
        let [x, y, z] = map read $ splitOn "," p :: [Int]
        pure (x, y, z)
  let scans = scanl' (\(s0, x) s1 ->
                            case overlap s0 s1 of
                              Just (s, y) -> (S.toList s, y)
                              Nothing -> (s0, x)
                    ) (head coords, ((0, 0, 0), [])) $ tail coords
  putStrLn $ intercalate "\n" $ map (\x -> show x ++ "\n") $ scans

