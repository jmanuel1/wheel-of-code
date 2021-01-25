{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Text as T
import Data.Char
import System.Random
import Graphics.UI.Fungen

data GameAttribute g = RandomGen g => GameAttribute g
data GameState = GameState [Char] T.Text

type WheelOfCodeObject = GameObject ()

type WheelOfCodeAction a g = IOGame (GameAttribute g) () GameState () a
type WheelOfCodeBinding g = InputBinding (GameAttribute g) () GameState ()

main :: IO ()
main = do
  let winConfig = ((0, 0), (250, 250), "Wheel of Code")
      bmpList = [("textures/background.bmp", Nothing)]
      gameMap = textureMap 0 50 50 250.0 250.0
      board = objectGroup "boardGroup" [] -- [createBoard]
      scoreBoard = objectGroup "scoreboardGroup" [] -- [createScoreboard]
      input = keyBindings
      (index, randGen') = randomR (0, length examples - 1) $ mkStdGen 4564
      example = examples !! index
      in funInit winConfig gameMap [board, scoreBoard] (GameState whitespace example) (GameAttribute randGen') input gameCycle (Timer 40) bmpList

createBoard :: WheelOfCodeObject
createBoard = let boardBound = [(-25, -6), (25, -6), (25, 6), (-25, 6)]
                  objPic = Basic (Polyg boardBound 1.0 1.0 1.0 Unfilled)
              in object "board" objPic False (125, 30) (0, 0) ()

createScoreboard :: WheelOfCodeObject
createScoreboard = let boardBound = [(-10, -10), (10, -10), (10, 10), (-10, 10)]
                       objPic = Basic (Polyg boardBound 1.0 1.0 1.0 Unfilled)
                      in object "scoreboard" objPic False (230, 230) (0, 0) ()

gameCycle :: WheelOfCodeAction () g
gameCycle = do
  GameState correctGuesses example <- getGameState
  when (example == hideCharactersExcept correctGuesses example)
    changeExample
  printOnScreen (show $ hideCharactersExcept correctGuesses example) Fixed9By15 (0, 30) 1.0 1.0 1.0

changeExample :: WheelOfCodeAction () g
changeExample = do
  GameAttribute randGen <- getGameAttribute
  let (index, randGen') = randomR (0, length examples - 1) randGen
  setGameState $ GameState whitespace $ examples !! index
  setGameAttribute $ GameAttribute randGen'

keyBindings :: [WheelOfCodeBinding g]
keyBindings = fmap (binding . toEnum) [0..127] where
  binding char = (Char char, Press, handler $ toLower char)
  handler char _ _ = do
    GameState correctGuesses example <- getGameState
    when (T.isInfixOf (T.singleton char) $ T.toLower example) $
      setGameState $ GameState (toUpper char:char:correctGuesses) example

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

whitespace = " \t\r\n\v"
