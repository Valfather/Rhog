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
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game {newTurn = True} --skip a turn
handleKeys (EventKey (Char 'p') Down _ _) game 
  | escMenu game       = game {escMenu = False}
  | not $ escMenu game = game {escMenu = True}
handleKeys (EventKey (Char x) Down _ _) game = newgame --trying to move ?
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
         allowedToMove (ax,ay) = whatIsThere (ax, ay) (oldmap, mwidth, mheight) `elem` [14,8,9] && not (thereBeMonsters (ax,ay) game) -- verify that nothing is blocking the player's path
    oldplayerpos = oldmap !! (fetchCoord (oldpposx,oldpposy)  (oldmap, mwidth, mheight))
    newMap = pictures [lastrender game, translate (fromIntegral $ oldpposx*23) (fromIntegral $ oldpposy*(-23)) (pictures [(preloadedTiles !! oldplayerpos)])]
    updatekGame game = game { cplayer = newplayer, newTurn = turnHappened, lastrender = newMap}
      where
        turnHappened
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
    allowedToMove (ax,ay) = whatIsThere (ax, ay) (oldmap, mwidth, mheight) `elem` [14,8,9] && not (thereBeMonsters (ax,ay) game) -- verify that nothing is blocking the actor's path
    execEachActor [] = []
    execEachActor (x:xs) = (execActor x ) : (execEachActor xs)
    execActor actor
      | wtomove actor = tryToMoveN actor
      | amove actor /= [] = tryToMoveA actor
      | otherwise = actor
      where
        tryToMoveA actor = if allowedToMove newcoord then actor {asees = drop 1 (amove actor), amove = drop 1 (amove actor), apos = newcoord} else actor {amove = drop 1 (amove actor)}
          where
            newcoord = (amove actor) !! 0
        tryToMoveN actor = if allowedToMove newcoord then (actor {apos = newcoord, ccycle = newccycle}) else actor {ccycle = newccycle}
          where
            (acx, acy) = apos actor
            cdir = (nmove actor) !! (ccycle actor)
            newccycle = ccycle actor + 1
            newcoord
              | cdir == 4 = (acx - 1, acy)
              | cdir == 2 = (acx, acy - 1)
              | cdir == 6 = (acx + 1, acy)
              | otherwise = (acx, acy + 1)
       
-- | Tells if there is an actor or a player at the given coordinates.
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
-- | Tells if the player or an actor is on a tile adjacent to the given coordinates
hasNeighbours :: Coord -> WholeGame -> Bool
hasNeighbours (wx,wy) game
  | (thereBeMonsters (wx+1,wy) game || thereBeMonsters (wx-1, wy) game || thereBeMonsters (wx, wy+1) game || thereBeMonsters (wx, wy-1) game) = True
  | otherwise = False
-- | Returns a list containing the neighbours of the given coordinates
neighboursOf :: Coord -> WholeGame -> [Coord]
neighboursOf (wx,wy) game = filter isNeighbour [(wx+1,wy),(wx-1,wy),(wx,wy+1),(wx,wy+1),(wx,wy-1)]
  where
    isNeighbour x
      | thereBeMonsters x game = True
      | otherwise = False
-- updates what the actor sees
updateActor ::  WholeGame ->Actor -> Actor
updateActor game actor
  | neighboursOf (apos actor) game == [] = actor
  | otherwise = actor {asees = neighboursOf (apos actor) game}
-- prepare the actor's actions
prepActor :: Actor -> Actor
prepActor actor = checkActorccycle $ doIt actor
  where
    doIt actor
      | behaviour actor == Curious = prepCurious actor
      | behaviour actor == Passive    = prepNeutral actor
      | otherwise = actor
    checkActorccycle actor
      | ccycle actor == (length $ nmove actor) = actor {ccycle = 0}
      | otherwise = actor

prepNeutral :: Actor -> Actor
prepNeutral actor
  | activity actor < 0  = actor
  | timer actor < (activity actor) = actor {timer = (timer actor) + 1, wtomove = False} 
  | otherwise = actor {timer = 0, wtomove = True}

prepCurious :: Actor -> Actor
prepCurious actor
  | asees actor == [] = prepNeutral actor
  | otherwise = actor {amove = asees actor, timer = 0}