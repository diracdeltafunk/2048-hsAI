module Game
( Board,
  StartingPos,
  generateBoard,
  monotonicity,
  jerk
) where
import Data.List
import Data.Maybe
type Tile = Maybe Int
type RowCol = [Tile]
type Board = [[Tile]]
type Coordinate
type StartingPos = ((Coordinate, Int), (Coordinate, Int))
coordToTile :: StartingPos -> Coordinate -> Tile
coordToTile (((ax, ay), av), ((bx, by), bv)) (x, y) | x == ax && y == ay = Just av
                                                    | x == bx && y == by = Just bv
                                                    otherwise = Nothing
generateBoard :: StartingPos -> Board
generateBoard s = let grid = map (zip [1, 2, 3, 4]) $ map repeat [0..3] in map (map (coordToTile s)) grid
groupBy2 :: [a] ->[(a, a)]
groupBy2 l = zip l $ tail l
max :: (Ord a) => (a, a) -> a
max (a, b) = if a > b then a else b
checkJump :: Tile -> Tile -> Int
checkJump Nothing Nothing = 1
checkJump Nothing _ = 1
checkJump _ Nothing = 0
checkJump (Just a) (Just b) = if a < b then 1 else 0
jumpScore :: RowCol -> Int --High Score is good
jumpScore r = sum $ map (uncurry checkJump) $ groupBy2 r
monotonicity :: Board -> Int --High Monotonicity is good
monotonicity b = (sum $ map jumpScore b) + (sum $ map (jumpScore.reverse) $ transpose b)
checkSmooth :: Tile -> Tile -> Double
checkSmooth Nothing Nothing = 0
checkSmooth Nothing _ = 0
checkSmooth _ Nothing = 1
checkSmooth (Just a) (Just b) = (fromInt (abs (a - b))) / (max (a, b))
smoothScore :: RowCol -> Double --Low smoothScore is good
smoothScore r = sum $ map (uncurry checkSmooth) $ groupBy2 r
jerk :: Board -> Double --Low Jerk is good
jerk b = (sum $ map smoothScore b) + (sum $ map smoothScore $ transpose b)
