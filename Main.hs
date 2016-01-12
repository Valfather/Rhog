module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Game
import Tools
import Levels
import Types
import Actors
import PlayerClasses

mgwidth, mgheight, offset :: Int
mgwidth = 23*34
mgheight = 23*30
offset = 100

window :: Display
window = InWindow "Test" (mgwidth, mgheight) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 30

firstplayer = loadPlayer hero
firstlevel = loadLevel level1
firstrender :: Picture
firstrender = pictures [firstlevel, firstplayer]

cGame :: WholeGame
cGame = Game { cplayer = hero {psees = actors level1}
             , clevel  = level1
             , newTurn = True
             , lastrender = firstrender
             }

updateGame :: Float -> WholeGame -> WholeGame
updateGame _ game = game

renderGame :: WholeGame -> Picture
renderGame game 
    | newTurn game = translate (-23*14) (23*15) (pictures [oldmap, newplayerpos, actorspos ])
    | otherwise = oldmap
    where
      actorspos = loadActors (psees (cplayer game))
      oldmap =  (lastrender game)
      newplayerpos =  (loadPlayer (cplayer game))
      
main :: IO ()
main = Graphics.Gloss.play window background fps cGame renderGame handleKeys updateGame

