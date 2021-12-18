import Prelude
import Data.List
import Data.Maybe
import qualified Data.Set as S

lx = 57
ly = -148
bx = 116
by = -198

correctTrajectories = catMaybes $ do
  (vx, vy) <- possibleVelocities
  let trajectory = expand (0, 0, vx, vy)
      notBeyond = takeWhile (\(tx, ty, _, _) -> tx <= bx && ty >= by) trajectory
  pure $ const (vx, vy) <$> find (\(tx, ty, _, _) -> tx >= lx && ty <= ly) notBeyond

  where
    expand (tx, ty, vx, vy) =
      (tx, ty, vx, vy):expand (tx + vx, ty + vy, max 0 (vx - 1), vy - 1)

    possibleVelocities =
      [ (x, y) | x <- [round $ sqrt 57..bx], y <- [by, by + 1..(abs by) - 1] ]


main = print $ length correctTrajectories
