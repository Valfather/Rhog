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
                                 , ccycle    = 0
                                 , wtomove   = False
                                 , nmove = [4,4,2,2,6,6,8,8]
                                 , asees     = [] }

prepActor :: Actor -> Actor
prepActor actor = doIt $ checkActorccycle actor
  where
    doIt actor
      | behaviour actor == Aggressive = prepAggressive actor
      | otherwise = actor
    checkActorccycle actor
      | ccycle actor == (length $ nmove actor) = actor {ccycle = 0}
      | otherwise = actor
prepNeutral :: Actor -> Actor
prepNeutral actor  
  | timer actor < 3 = actor {timer = (timer actor) + 1} 
  | otherwise = actor {timer = 0, wtomove = True}
prepAggressive :: Actor -> Actor
prepAggressive actor
  | asees actor == [] = prepNeutral actor
  | otherwise = actor