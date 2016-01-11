module Tools where
import Graphics.Gloss
import Graphics.Gloss.Game
import Types
import System.Random
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
    turnToPic levelmap = pictures [translate (fromIntegral (theight*xpos)) (fromIntegral (twidth*ypos)) (png ("Tilesets/"++tile++".png")) | (tile,xpos,ypos) <- levelmapfixed]
      where -- for debugging purposes, ignore the following
        levelmapfixed = levelmap
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
        |y < lwidth = ((transformap x), y, z) : makeMap xs tset (y+1) z lwidth
        |y == lwidth = ((transformap x), 0, z+1)  : makeMap xs tset 1 (z+1) lwidth
        where
          transformap x
           |x == 0 = "wall1_"     ++ tset
           |x == 1 = "wall2_"     ++ tset
           |x == 2 = "wall3_"     ++ tset
           |x == 3 = "wallCH_"    ++ tset
           |x == 4 = "water"
           |x == 5 = "fire"
           |x == 6 = "acidwater"
           |x == 7 = "wallCV_"    ++ tset
           |x == 8 = "doorOH_"    ++ tset
           |x == 9 = "doorOV_"    ++ tset
           |x == 10 = "doorCV_"   ++ tset
           |x == 11 = "doorCH_"   ++ tset
           |x == 12 = "stairsD"
           |x == 13 = "stairsU"
           |x == 14 = "floor_"    ++ tset
           |x == 15 = ""
--  | Turns a player into a picture useable by gloss
loadPlayer :: Player -> Picture
loadPlayer player = translate (fromIntegral (px*23))  (fromIntegral (py*23)) (png pnfile)
  where
    (px, py) = ppos player
    pnfile = "Player/" ++ (cclass player) ++ ".png"
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
    newplayer -- ^ check if the tile you're trying to go to is a floor, door, or navigable tile. otherwise, you're stuck in place
       | x == 'q'  = if whatIsThere (oldpposx - 1, oldpposy) (oldmap, mwidth, mheight) `elem` [14,8,9] then (cplayer game) {ppos = (oldpposx - 1, oldpposy) } else cplayer game
       | x == 'd'  = if whatIsThere (oldpposx + 1, oldpposy) (oldmap, mwidth, mheight) `elem` [14,8,9] then (cplayer game) {ppos = (oldpposx + 1, oldpposy) } else cplayer game
       | x == 's'  = if whatIsThere (oldpposx, oldpposy - 1) (oldmap, mwidth, mheight) `elem` [14,8,9] then (cplayer game) {ppos = (oldpposx, oldpposy - 1) } else cplayer game
       | x == 'z'  = if whatIsThere (oldpposx, oldpposy + 1) (oldmap, mwidth, mheight) `elem` [14,8,9] then (cplayer game) {ppos = (oldpposx, oldpposy + 1) } else cplayer game
       | otherwise = cplayer game
    oldplayerpos = transformap (oldmap !! (fetchCoord (oldpposx,oldpposy)  (oldmap, mwidth, mheight)))
    transformap x
       |x == 0 = "wall1_"     ++ tset
       |x == 1 = "wall2_"     ++ tset
       |x == 2 = "wall3_"     ++ tset
       |x == 3 = "wallCH_"    ++ tset
       |x == 4 = "water"
       |x == 5 = "fire"
       |x == 6 = "acidwater"
       |x == 7 = "wallCV_"    ++ tset
       |x == 8 = "doorOH_"    ++ tset
       |x == 9 = "doorOV_"    ++ tset
       |x == 10 = "doorCV_"   ++ tset
       |x == 11 = "doorCH_"   ++ tset
       |x == 12 = "stairsD"
       |x == 13 = "stairsU"
       |x == 14 = "floor_"    ++ tset
       |x == 15 = ""
    newMap = pictures [lastrender game, translate (fromIntegral $ oldpposx*23) (fromIntegral $ oldpposy*23) (png ("Tilesets/"++oldplayerpos++".png"))]
    updatekGame game = game { cplayer = newplayer, justmoved = True, lastrender = newMap}
handleKeys _ game = game -- ^ in all other cases, you're fucked 