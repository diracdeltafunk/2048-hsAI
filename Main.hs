module Main where
import Game
payoff :: Board -> Double
payoff b = (fromInt (monotonicity b)) - (jerk b)
options :: Integer -> Board -> [Board]
options i b = unfoldr (\(n, s) -> if n > i then Nothing else Just (s, (n+1, concat $ map ((zipWith ($) [left, right, up, down]).repeat) s))) (0, [b])
