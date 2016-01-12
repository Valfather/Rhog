module Preload where
import Types
import Graphics.Gloss
import Graphics.Gloss.Game
tilefolder :: FilePath
tilefolder = "Tilesets/"
playerfolder :: FilePath
playerfolder = "Player/"
actorfolder :: FilePath
actorfolder = "Monsters/"
-- | all map tiles. 0-15 temple
preloadedTiles :: [Picture]
preloadedTiles =   [ png (tilefolder ++ "wall1_temple.png") 
                   , png (tilefolder ++ "wall2_temple.png")
                   , png (tilefolder ++ "wall3_temple.png")
                   , png (tilefolder ++ "wallCH_temple.png")
                   , png (tilefolder ++ "water.png")
                   , png (tilefolder ++ "fire.png")
                   , png (tilefolder ++ "acidwater.png")
                   , png (tilefolder ++ "wallCV_temple.png")
                   , png (tilefolder ++ "doorOH_temple.png")
                   , png (tilefolder ++ "doorOV_temple.png")
                   , png (tilefolder ++ "doorCV_temple.png")
                   , png (tilefolder ++ "doorCH_temple.png")
                   , png (tilefolder ++ "stairsD.png")
                   , png (tilefolder ++ "stairsU.png")
                   , png (tilefolder ++ "floor_temple.png")
                   , pictures [] ]
-- | all player tiles
preloadedPlayer :: [Picture]
preloadedPlayer =  [ png (playerfolder ++ "Hero.png")
                   , png (playerfolder ++ "Mage.png")
                   , png (playerfolder ++ "Fighter.png")
                   , png (playerfolder ++ "Archer.png")]
preloadedActors :: [Picture]
preloadedActors =  [ png (actorfolder ++ "Salamander.png")]
                   