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
type LevelMap = [(String,Int,Int)]
type NewLevelMap = ([Int],Int,Int)
type Coord = (Int,Int)
-- contains the whole game structur
data WholeGame = Game { cplayer    :: Player
                      , clevel     :: Level
                      , justmoved  :: Bool
                      , lastrender :: Picture
                      } deriving (Show, Eq)