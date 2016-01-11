
-- | Fill a Map with a specific element. See makeMap to know what tiles correspond to the numbers.   
fillMap :: [Int]   -- ^ original map
         -> Int    -- ^ wanted element
         -> [Int]  -- ^ end result
fillMap levelmap element = [element | x <- levelmap]
-- | replace the given element by another in a list
repElement :: [Int]
           -> Int  -- ^ element to replace
           -> Int  -- ^ element to replace with
           -> [Int] 
repElement lmap whatel whatel = map (repThing whatel whatel) lmap
where
  repThing x y z
    | z == x = y
    | otherwise = z 
-- | returns True if there is no place in the map for a new room.
isComplete :: NewLevelMap
           -> Bool
isComplete = True
-- | returns True if there is place for a room at the given space
placeForRoom :: (Coord, Coord) -- ^ top left corner and bottom right corner of the room
             -> NewLevelMap
             -> Bool
placeForRoom ((tlcx, tlcy), (brcx, brcy)) (grid, width, height)
  | 14 `elem` roomToBe -- ^ check if there is a floor tile in the wall we want to dig
  where
    
-- | returns True if the given tile is a wall
isWall :: Coord
       -> NewLevelMap
       -> Bool
isWall coordinates (grid, lwidth, lheight)
  | grid !! (fetchCoord coordinates lmap ) `elem` [0,1,2] = isAdjacent coordinates lmap 14 -- ^ check if it's a wall tile or not, if yes check if it's adjacent to a floor tile
  | otherwise = False
    where
	  (x,y) = coordinates
	  (grid, width, height) = lmap
	  isAdjacent coordinates lmap el
	    | ((grid !! (fetchCoord (x+1,y) lmap) == l || (grid !! (fetchCoord (x-1,y) lmap) == l || (grid !! (fetchCoord (x,y+1) lmap) == l || (grid !! (fetchCoord (x,y-1) lmap) == l ) = True
		| otherwise = False
-- | Create a map of a given size filled with 15th element
createMap :: (Int, Int) -- ^ the size of the map
          -> [Int]      -- ^ end result
createMap (x,y) = replicate (x*y) 15
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
-- | Tells if the element corresponding to the coords is in a room of given tile
isInRoom :: (Coord, Coord) -- ^ top left and bottom right corners of the room
         -> Coord          -- ^ coordinates of the tile
         -> Bool
isInRoom ((tlcx, tlcy) , (brcx, brcy)) (x,y) 
   | ((x >=  tlcx) && (x < brcx) && (y >= tlcy ) && (y < brcy)) = True
   | otherwise = False
-- | make a rectangular room from given coord in given map
makeRoom :: NewLevelMap
         -> Coord          -- ^ position of the top left corner
         -> (Int,Int)      -- ^ Size of the room
         -> NewLevelMap
makeRoom (solemap, width, height) (cx, xy) (sx, sy) = createRoom (tlc, brc) lmap (0,0)
where
  coordinates = (cx, xy)
  (tlc, brc) = (coordinates, ((cx+sx-1), (cx+sy-1)) -- ^ Room
  lmap = (solemap, width, height)
  there = fetchCoord lmap coordinates
  createRoom (tlc, brc) ((x:xs), width height) (y,z)
    |(y,z) == (width, height) = []
    |otherwise = appendToMap (tlc, brc) ((x:xs), width height) (y,z)
    where
      appendToMap (tlc, brc) ((x:xs), width height) (y,z)
       |isInRoom (y,z) (tlc, brc) == 0 = 14 :  appendToMap (tlc, brc) ((xs), width height) (y+1,z)
       |otherwise                      = 16 :  appendToMap (tlc, brc) ((xs), width height) (y,z) -- ^ prepare "repElement  lmap 16 primwall"
-- | contains the algorithm.
makeDungeon :: NewLevelMap
            -> NewLevelMap
makeDungeon (grid, width, height) = firststage
 where 
  secondstage nlmap = pickWall lmap
  firststage = makeRoom lmap ((width `div` 2), (height `div` 2)) (sizex sizey)
  lmap = (grid, width, height)
  sizex = getStdRandom(randomR (3, width `div` 2,)
  sizey = getStdRandom(randomR (3, height `div` 2 )
-- | Randomly generates a level with a haskell version of Mike Anderson's dungeon-building algorithm (www.roguebasin.com) which is most suited for caves or sunken keeps.
generateLevel :: String     -- ^ Name of the map
              -> (Int,Int)  -- ^ The size of the map
              -> String     -- ^ The Tileset for the map
              -> Level      -- ^ EndResult.   
generateLevel nname (x,y) tset = Level { name = nname , height = x, width = y, tileset = tset, tileheight = 23, tilewidth = 23, lvlmap = endmap }
  where 
    primwall = getStdRandom(randomR (0,2))
    firstmap = createMap (x,y))
    endmap = makeDungeon(firstmap, x, y)
