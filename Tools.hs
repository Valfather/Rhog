module Tools where
import Graphics.Gloss
import Graphics.Gloss.Game
import Types
import Actors
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
         allowedToMove (ax,ay) = whatIsThere (ax, ay) (oldmap, mwidth, mheight) `elem` [14,8,9] && not (thereBeMonsters (ax,ay) game)
    oldplayerpos = oldmap !! (fetchCoord (oldpposx,oldpposy)  (oldmap, mwidth, mheight))
    newMap = pictures [lastrender game, translate (fromIntegral $ oldpposx*23) (fromIntegral $ oldpposy*(-23)) (pictures [(preloadedTiles !! oldplayerpos)])]
    updatekGame game = game { cplayer = newplayer, newTurn = turnhappened, lastrender = newMap}
      where
        turnhappened
          | x `elem` ['q','d','s','z'] = True
          | otherwise = False
handleKeys _ game = game   -- in all other cases, nothing happens
-- | prepare the actors' actions this turn
prepActors :: WholeGame -> WholeGame
prepActors game = game { clevel = tlevel {actors = prepEachActor curActors}}
  where
    tlevel = clevel game
    curActors = actors tlevel
    prepEachActor [] = []
    prepEachActor (x:xs) = (prepActor x) : (prepEachActor xs)
-- | execute said action
execActors :: WholeGame -> WholeGame
execActors game = game {clevel = tlevel {actors = execEachActor curActors}}
  where
    tlevel = clevel game  
    curActors = actors (clevel game)
    oldmap = lvlmap (clevel game)
    mheight = height (clevel game)
    mwidth = width (clevel game)
    tset = tileset (clevel game)
    allowedToMove (ax,ay) = whatIsThere (ax, ay) (oldmap, mwidth, mheight) `elem` [14,8,9] && not (thereBeMonsters (ax,ay) game)
    execEachActor [] = []
    execEachActor (x:xs) = (execActor x ) : (execEachActor xs)
    execActor actor
     | wtomove actor = tryToMoveRd actor
     | otherwise = actor
     where
       tryToMoveRd actor = if allowedToMove newcoord then (actor {apos = newcoord, ccycle = newccycle}) else actor
         where
           (acx, acy) = apos actor
           cdir = (nmove actor) !! (ccycle actor)
           newccycle = ccycle actor+ 1
           newcoord
             |cdir == 4 = (acx - 1, acy)
             |cdir == 2 = (acx, acy - 1)
             |cdir == 6 = (acx + 1, acy)
             |otherwise = (acx, acy + 1)
       
-- | tells if there is an actor or a player at the given coordinates.
thereBeMonsters :: Coord -> WholeGame -> Bool
thereBeMonsters (wx,wy) game
  | actorsThere || playerThere = True
  | otherwise = False
  where 
    actorsThere 
      | (wx,wy) `elem` (coordList hactors) = True
      | otherwise = False
      where
        hactors = actors (clevel game)
        coordList [] = []
        coordList (x:xs) = (apos x) : (coordList xs)
    playerThere
      | (wx,wy) == (ppos hplayer) = True
      | otherwise = False
      where 
        hplayer = cplayer game