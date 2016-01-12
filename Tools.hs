module Tools where
import Graphics.Gloss
import Graphics.Gloss.Game
import Types
import System.Random
import Preload
-- | Returns the position of the element in the list that corresponds to its coordinates in the lvlmap
fetchCoord :: Coord -- ^ Coordinates
           -> NewLevelMap -- ^ Map
           -> Int -- ^ endresult : position 
fetchCoord (x,y) (_, width, _) = (y*width+x)
-- | returns the coordinates in the lvlmap  of the nth element of the list
fetchPos :: Int         -- ^ n:  position of the element in the list
         -> NewLevelMap -- ^  Map
         -> Coord
fetchPos n (grid, width, height) = (xpos, ypos)
  where
   ypos = n `div` width
   xpos = n - ypos
-- | Turns a level into a picture useable by gloss
loadLevel :: Level -> Picture
loadLevel level = turnToPic (mkMap level)
  where
   theight = tileheight level
   twidth = tilewidth level
   turnToPic levelmap = pictures [translate (fromIntegral (theight*xpos)) (fromIntegral (twidth*ypos)) (preloadedTiles !! tile ) | (tile,xpos,ypos) <- levelmapfixed]
      where -- for debugging purposes, ignore the following
       levelmapfixed =[(tile, xpos,-ypos) | (tile,xpos,ypos) <- levelmap]
-- | Subfunction of loadLevel
mkMap :: Level-> LevelMap
mkMap maplev = makeMap themap tset 0 0 lwidth
    where
     themap = lvlmap maplev
     lwidth = width maplev
     tset = tileset maplev
-- | Subfunction of mkMap
makeMap :: [Int] -> String -> Int -> Int -> Int -> LevelMap
makeMap [] _ _ _ _ = []
makeMap (x:xs) tset y z lwidth
        | y < lwidth = (x, y, z) : makeMap xs tset (y+1) z lwidth
        | y == lwidth = (x, 0, z+1)  : makeMap xs tset 1 (z+1) lwidth
--  | Turns a player into a picture useable by gloss
loadPlayer :: Player -> Picture
loadPlayer player = translate (fromIntegral (px*23))  (fromIntegral (py*(-23))) (preloadedPlayer !! (cclass player))
  where
    (px, py) = ppos player
-- |Turns all monsters the player can see to pictures
loadActors :: [Actor] -> Picture
loadActors [] = pictures []
loadActors seenactors = pictures (toPictures seenactors)
    where
     toPictures [] = []
     toPictures (x:xs) = (translate (fromIntegral (ax*23)) (fromIntegral (ay*(-23))) (preloadedActors !! whatactor)) : toPictures xs
       where
        (ax, ay)  = (apos x)
        whatactor = aid x
   
-- | check what tile is at that coord
whatIsThere :: Coord
            -> NewLevelMap
            -> Int
whatIsThere there lmap = grid !! (fetchCoord there lmap)
  where
    (grid, x, y) = lmap 
-- Handle înput
handleKeys :: Event -> WholeGame -> WholeGame
handleKeys (EventKey (Char x) Down _ _) game = newgame
  where
    (oldpposx, oldpposy) = ppos (cplayer game)  
    oldmap = lvlmap (clevel game)
    mheight = height (clevel game)
    mwidth = width (clevel game)
    tset = tileset (clevel game)
    newgame = updatekGame game
    newplayer --  check if the tile you're trying to go to is a floor, door, or navigable tile. otherwise, you're stuck in place
       | x == 'q'  = if allowedToMove (oldpposx - 1, oldpposy)  then (cplayer game) {ppos = (oldpposx - 1, oldpposy) } else cplayer game
       | x == 'd'  = if allowedToMove (oldpposx + 1, oldpposy)  then (cplayer game) {ppos = (oldpposx + 1, oldpposy) } else cplayer game
       | x == 's'  = if allowedToMove (oldpposx, oldpposy + 1)  then (cplayer game) {ppos = (oldpposx, oldpposy + 1) } else cplayer game
       | x == 'z'  = if allowedToMove (oldpposx, oldpposy - 1)  then (cplayer game) {ppos = (oldpposx, oldpposy - 1) } else cplayer game
       | otherwise = cplayer game
       where
         allowedToMove (ax,ay) = whatIsThere (ax, ay) (oldmap, mwidth, mheight) `elem` [14,8,9]
    oldplayerpos = oldmap !! (fetchCoord (oldpposx,oldpposy)  (oldmap, mwidth, mheight))
    newMap = pictures [lastrender game, translate (fromIntegral $ oldpposx*23) (fromIntegral $ oldpposy*(-23)) (pictures [(preloadedTiles !! oldplayerpos)])]
    updatekGame game = game { cplayer = newplayer, newTurn = turnhappened, lastrender = newMap}
      where
        turnhappened
          | x `elem` ['q','d','s','z'] = True
          | otherwise = False
handleKeys _ game = game   -- in all other cases, nothing happens