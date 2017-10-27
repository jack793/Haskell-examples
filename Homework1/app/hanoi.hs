type Peg  = [Int]
data Game = Game { a :: Peg, b :: Peg, c :: Peg } deriving (Show)
type Sel  = Game -> Peg

initPegs :: Int -> Game
initPegs n = Game { a = [n, (n-1) .. 1], b = [], c = [] }

hanoi :: Int -> Game -> Game

hanoi 1 Game { a = a, b = b, c = c } =
  Game { a = take k a, b = b ++ drop k a, c = c }
  where k = length a - 1

hanoi n game =
  foldl transform game moves
  where
    transform game (k, selectors) =
      arrange selectors $ hanoi k $ arrange selectors game

    arrange (x, y, z) game =
      Game { a = x game, b = y game, c = z game }

    moves = [
        (n-1, (a, c, b)),
        (1,   (a, b, c)),
        (n-1, (c, b, a))
      ]

main = do
  let n = 6
  let pegs = initPegs n
  print $ hanoi n pegs