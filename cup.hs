main = print ""

cup ml = \msg -> msg ml

coffeeCup = cup 12

getMl aCup = aCup (\ml -> ml)

drink aCup mlDrunk =
  if mlDiff >= 0 then
    cup mlDiff
  else
    cup 0
  where ml = getMl aCup
        mlDiff = ml - mlDrunk

isEmpty aCup = getMl aCup == 0
