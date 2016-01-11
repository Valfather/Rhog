module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Game
import Tools
import Levels
import Types
import PlayerClasses

mgwidth, mgheight, offset :: Int
mgwidth = 300
mgheight = 300
offset = 100

window :: Display
window = InWindow "Test" (mgwidth, mgheight) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 30

firstplayer = loadPlayer mage
firstlevel = loadLevel level1
firstrender :: Picture
firstrender = pictures [firstlevel, firstplayer]

cGame :: WholeGame
cGame = Game { cplayer = mage
             , clevel  = level1
             , justmoved = True
             , lastrender = firstrender
             }

updateGame :: Float -> WholeGame -> WholeGame
updateGame _ game = game

renderGame :: WholeGame -> Picture
renderGame game 
    | justmoved game = pictures [oldmap, newplayerpos]
    | otherwise = oldmap
    where
      oldmap = translate (-23*10) (-23*10) (lastrender game)
      newplayerpos = translate (-23*10) (-23*10) (loadPlayer (cplayer game))
      
main :: IO ()
main = Graphics.Gloss.play window background fps cGame renderGame handleKeys updateGame

