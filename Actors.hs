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
                                 , behaviour = Curious
                                 , timer     = 0
                                 , ccycle    = 0
                                 , wtomove   = False
                                 , activity  = 3
                                 , nmove     = [4,4,2,2,6,6,8,8]
                                 , amove     = []
                                 , asees     = [] }