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
                   } deriving (Show, Eq, Read)

data Player = Player { pname   :: String
                     , ppos    :: (Int, Int)
                     , health  :: (Int, Int)
                     , mana    :: (Int, Int)                    
                     , cclass  :: String
                     } deriving (Show, Eq, Read)  
                     
data Actor = Actor  { aname     :: String
                    , apos      :: (Int, Int)
                    , ahealth   :: (Int, Int)
                    , amana     :: (Int, Int)
                    , behaviour :: Behaviour
                    , sees      :: [Player]
                    } deriving (Show, Eq)

type LevelMap = [(String,Int,Int)]

data Behaviour = Aggressive | Friendly | Passive deriving (Show, Eq)
type NewLevelMap = ([Int],Int,Int)
type Coord = (Int,Int)

--  | contains the whole game structure
data WholeGame = Game { cplayer    :: Player
                      , clevel     :: Level
                      , newTurn  :: Bool
                      , lastrender :: Picture
                      } deriving (Show, Eq)
