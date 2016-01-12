module Types where
import Graphics.Gloss

data Level = Level { lname       :: String
                   , height      :: Int
                   , width       :: Int
                   , tileset     :: FilePath
                   , tileheight  :: Int
                   , tilewidth   :: Int
                   , spos        :: (Int,Int)
                   , lvlmap      :: [Int]
                   , actors      :: [Actor]
                   } deriving (Show, Eq)

data Player = Player { pname   :: String
                     , ppos    :: (Int, Int)
                     , health  :: (Int, Int)
                     , mana    :: (Int, Int)                    
                     , cclass  :: Int
                     , psees   :: [Actor] 
                     } deriving (Show, Eq)  
                     
data Actor = Actor  { aname     :: String
                    , aid       :: Int
                    , apos      :: (Int, Int)
                    , ahealth   :: (Int, Int)
                    , amana     :: (Int, Int)
                    , behaviour :: Behaviour
                    , wtomove   :: Bool
                    , timer     :: Int
                    , ccycle    :: Int
                    , nmove     :: [Int]
                    , activity  :: Int
                    , amove     :: [Coord]
                    , asees     :: [Coord]
                    } deriving (Show, Eq)

type LevelMap = [(Int,Int,Int)] -- ^ whatTile, wherex, wherey

data Behaviour = Curious | Friendly | Passive deriving (Show, Eq)
type NewLevelMap = ([Int],Int,Int)
type Coord = (Int,Int)

--  | contains the whole game structure
data WholeGame = Game { cplayer     :: Player
                      , clevel      :: Level
                      , newTurn     :: Bool
                      , escMenu     :: Bool
                      , lastrender  :: Picture
                      , turncount   :: Int
                      , newTurnDone :: Bool
                      } deriving (Show, Eq)
