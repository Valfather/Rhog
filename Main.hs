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

roghName = "Rogh v0.0.17"

window :: Display
window = InWindow roghName (mgwidth, mgheight) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 30

firstplayer = loadPlayer hero
firstlevel = loadLevel level1
firstrender :: Picture
firstrender = (pictures [firstlevel, firstplayer])


 
cGame :: WholeGame
cGame = Game { cplayer = hero {psees = actors level1}
             , clevel  = level1
             , newTurn = False
             , newTurnDone = True
             , escMenu = False
             , lastrender = firstrender
             , turncount = 0
             }

updateGame :: Float -> WholeGame -> WholeGame
updateGame _ game
    | newTurn game = finalize $ actActors $ updateSight newgame
    | otherwise = game 
    where
      newgame = game {turncount = (turncount game) +1 }
      actActors game = (execActors (prepActors game)) {newTurnDone = True}
      finalize game = game {newTurn = False}
      updateSight game = game {clevel = (clevel game) {actors = updatedActors}, cplayer = (cplayer game) {psees = actors (clevel game)}}
        where
          updatedActors = map (updateActor game) ( actors $ clevel game)
renderGame :: WholeGame -> Picture
renderGame game 
  | escMenu game = renderMenu game
  | otherwise = renderMap game
  where
    renderMap game
      | newTurnDone game = scale 2 2 (translate (-23*fromIntegral(ppx)) (23*fromIntegral(ppy)) (pictures [oldmap, newplayerpos, actorspos]))
      | otherwise = oldmap
        where
          actorspos = loadActors (psees (cplayer game))
          oldmap =  (lastrender game)
          newplayerpos =  loadPlayer (cplayer game)
          (ppx, ppy) = ppos (cplayer game)
    renderMenu game = color white $ pictures [ gameName, playerName, playerInfo]
      where
        gameName     =  translate (-23*17) (23*13.5) $ (scale 0.3 0.3 $ text roghName)
        playerName   =  translate (-23*17) (23*3)    $ (scale 0.45 0.45 $ text $ pname (cplayer game))
        playerInfo   =  translate (-23*16)  (0)      $ (pictures [playerHealth, translate 0 (-65) playerMana])
        playerHealth =  scale 0.32 0.32 $ text ("Health : " ++ (show $ health $ cplayer game))
        playerMana   =  scale 0.32 0.32 $ text ("Mana : "   ++ (show $ mana   $ cplayer game))
main :: IO ()
main = Graphics.Gloss.play window background fps cGame renderGame handleKeys updateGame

