module Actors where
import Types
hero :: Player
hero = Player { pname   = "TestHeroine"
              , ppos    = (15,27)
              , health  = (10,10)
              , mana    = (0,0)                   
              , cclass  = 0
              , psees = []
              }
generateSalamander :: Coord -> Actor
generateSalamander there = Actor { aname     = "Salamander"
                                 , aid       = 0
                                 , apos      = there
                                 , ahealth   = (15,15)
                                 , amana     = (5,5)
                                 , behaviour = Aggressive
                                 , timer     = 0
                                 , asees     = [] }

moveActor :: Actor -> Actor
moveActor actor
  | behaviour actor == Aggressive = moveAggressive actor
  | otherwise = actor  
moveAggressive :: Actor -> Actor
moveAggressive actor
  | asees actor == [] = actor
  | otherwise = actor