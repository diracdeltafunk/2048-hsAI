module Game
( Board,
  StartingPos,
  generateBoard,
  monotonicity,
  jerk,
  left,
  right,
  up,
  down,
  add,
  win,
  lose
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
cGrid :: [[Coordinate]]
cGrid = map (zip [1, 2, 3, 4]) $ map repeat [0..3]
grid :: Board -> [[(Coordinate, Int)]]
grid b = zipWith (\gr br -> zip gr br) cGrid b
generateBoard :: StartingPos -> Board
generateBoard s = map (map (coordToTile s)) cGrid
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
squishL, squishR :: Int -> RowCol -> RowCol
squishL l Nothing:r = (squish l r) ++ [Nothing]
squishL l e:es = e:(squish l es)
squishR a b = reverse $ squishL a $ reverse b
collapseL, collapseR :: RowCol -> RowCol
collapseL [x] = [x]
collapseL Nothing:r = Nothing:(collapseL r)
collapseL e:es = if e == head es then [2*e, Nothing] ++ (collapseL (tail es)) else e:(collapseL es)
collapseR l = reverse $ collapseL $ reverse l
left, right, up, down :: Board -> Board
left b = squishL $ map collapseL $ squishL b
right b = squishR $ map collapseR $ squishR b
up b = transpose $ left $ transpose b
down b = transpose $ right $ transpose b
insert :: Tile -> Board -> Coordinate -> Board
insert t b c = map (map (\(cn, v) -> if cn == c then t else v)) $ grid b
empties :: Board -> [Coordinate]
empties b = map fst $ concat $ map (filter ((==Nothing).snd)) $ grid b
add :: Tile -> Board -> [Board]
add t b = map (insert t b) $ empties b
win, lose :: Board -> Bool
win b = any $ map any $ map (map (==(Just 2048))) b
lose b = all $ map (==b) $ zipWith ($) [left, right, up, down] $ repeat b
