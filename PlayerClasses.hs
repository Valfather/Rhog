module PlayerClasses where
import Types
-- | Promotes a player to Mage
promoteMage :: Player -> Player
promoteMage player = player {cclass = "Mage", health = (12, 12), mana = (20, 20)}
-- | Promotes a player to Fighter
promoteFighter :: Player -> Player
promoteFighter player = player {cclass = "Fighter", health = (18, 18), mana = (0, 0)}
-- | Promotes a player to Archer
promoteArcher :: Player -> Player
promoteArcher player = player {cclass = "Archer", health = (12, 12), mana = (0, 0)}
