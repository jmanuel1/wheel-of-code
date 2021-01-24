module Main where

import qualified Data.Text as T
import Graphics.UI.Fungen

newtype GameAttribute = Score Int
data GameState = GameState [Char] T.Text

type WheelOfCodeObject = GameObject ()

type WheelOfCodeAction a = IOGame () () GameState () a
type WheelOfCodeBinding = InputBinding () () GameState ()

main :: IO ()
main = do
  let winConfig = ((0, 0), (250, 250), "Wheel of Code")
      bmpList = [("textures/background.bmp", Nothing)]
      gameMap = textureMap 0 50 50 250.0 250.0
      board = objectGroup "boardGroup" [] -- [createBoard]
      scoreBoard = objectGroup "scoreboardGroup" [] -- [createScoreboard]
      input = keyBindings
      in funInit winConfig gameMap [board, scoreBoard] (GameState [] $ head examples) () input gameCycle (Timer 40) bmpList

createBoard :: WheelOfCodeObject
createBoard = let boardBound = [(-25, -6), (25, -6), (25, 6), (-25, 6)]
                  objPic = Basic (Polyg boardBound 1.0 1.0 1.0 Unfilled)
              in object "board" objPic False (125, 30) (0, 0) ()

createScoreboard :: WheelOfCodeObject
createScoreboard = let boardBound = [(-10, -10), (10, -10), (10, 10), (-10, 10)]
                       objPic = Basic (Polyg boardBound 1.0 1.0 1.0 Unfilled)
                      in object "scoreboard" objPic False (230, 230) (0, 0) ()

gameCycle :: WheelOfCodeAction ()
gameCycle = do
  GameState correctGuesses example <- getGameState
  printOnScreen (show $ hideCharactersExcept correctGuesses example) Fixed9By15 (0, 30) 1.0 1.0 1.0

keyBindings :: [WheelOfCodeBinding]
keyBindings = fmap binding ['a'..'z'] where
  binding char = (Char char, Press, handler char)
  handler char _ _ = do
    GameState correctGuesses example <- getGameState
    when (T.isInfixOf (T.singleton char) example) $
      setGameState $ GameState (char:correctGuesses) example

hideCharactersExcept :: [Char] -> T.Text -> T.Text
hideCharactersExcept xs = T.map hide where
  hide char = if char `elem` xs then char else '_'

isGuessCorrect :: T.Text -> Char -> Int -> Bool
isGuessCorrect string char index =
  char == T.index string index

examples = fmap T.pack
  [ "print('Hello world!')"
  , "def factorial(n):\n\tif n < 0:\n\t\treturn 1\n\telse:\n\t\treturn n*factorial(n-1)"
  , "def triangular(n): return n**2 + n"
  , "def fib(n): return fib(n-1) + fib(n-2) if n > 2 else 1" ]
