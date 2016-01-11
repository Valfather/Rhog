module Actors where
import Types
hero :: Player
hero = Player { pname   = "TestHeroine"
              , ppos    = (15,27)
              , health  = (10,10)
              , mana    = (0,0)                   
              , cclass  = "Hero"
              , psees = []
              }
generateSalamander :: Coord -> Actor
generateSalamander there = Actor { aname     = "Salamander"
                                 , apos      = there
                                 , ahealth   = (15,15)
                                 , amana     = (5,5)
                                 , behaviour = Aggressive
                                 , asees      = [] }
