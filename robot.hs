main =
  print ""

robot :: (String, Int, Int) -> ((String, Int, Int) -> result) -> result
robot (n,a,h) = \message -> message (n,a,h)

name :: (String, Int, Int) -> String
name (n,_,_) = n
attack :: (String, Int, Int) -> Int
attack (_,a,_) = a
hp :: (String, Int, Int) -> Int
hp (_,_,hp) = hp


getName :: ((((String, Int, Int) -> String) -> String)) -> String
getName aRobot = aRobot name

getAttack :: ((((String, Int, Int) -> Int) -> Int)) -> Int
getAttack aRobot = aRobot attack

getHP :: ((((String, Int, Int) -> Int) -> Int)) -> Int
getHP aRobot = aRobot hp

setName :: (((String, Int, Int) -> ((String, Int, Int) -> result) -> result) -> t ) -> String -> t
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))

setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))

setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack:" ++ (show a) ++ " hp:"++ (show h))

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))

killerRobot :: ((String, Int, Int) -> result) -> result
killerRobot = robot ("Kill3r",25,200)
nicerRobot :: ((String, Int, Int) -> result) -> result
nicerRobot = setName killerRobot "kitty"
gentlerRobot :: ((String, Int, Int) -> result) -> result
gentlerRobot = setAttack killerRobot 5
softerRobot :: ((String, Int, Int) -> result) -> result
softerRobot = setHP killerRobot 50


robots = [ killerRobot, nicerRobot, gentlerRobot, softerRobot]

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                  then getAttack aRobot
                  else 0

getHps robots = map getHP robots
