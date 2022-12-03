module Day2 where
import AdventLibrary (getFileContent)

main = do
  matches <- lines <$> getFileContent "Day2Input"
  let result1 = sumMaybes $ map (computePointsFromGestures . parseMatch) matches
      result2 = sumMaybes $ map (computePointsFromGestureAndResult . parseMatchResult) matches
  print (result1, result2)

data Gesture = Rock | Paper | Scissors

parseMatch :: String -> (Maybe Gesture, Maybe Gesture)
parseMatch rawMatch = case words rawMatch of
  [theirs, mine] -> (parseTheirs theirs, parseMine mine)
  _ -> (Nothing, Nothing)

parseMatchResult :: String -> (Maybe Gesture, Maybe Result)
parseMatchResult rawMatch = case words rawMatch of
  [theirs, mine] -> (parseTheirs theirs, parseResult mine)
  _ -> (Nothing, Nothing)

sumMaybes :: Num a => [Maybe a] -> Maybe a
sumMaybes = foldl addMaybes (Just 0)
  where 
    addMaybes _ Nothing = Nothing 
    addMaybes Nothing _ = Nothing 
    addMaybes (Just a) (Just b) = Just (a + b)


parseTheirs :: String -> Maybe Gesture
parseTheirs "A" = Just Rock
parseTheirs "B" = Just Paper
parseTheirs "C" = Just Scissors
parseTheirs _ = Nothing

parseMine :: String -> Maybe Gesture
parseMine "X" = Just Rock
parseMine "Y" = Just Paper
parseMine "Z" = Just Scissors
parseMine _ = Nothing

data Result = Lose | Draw | Win 

parseResult :: String -> Maybe Result
parseResult "X" = Just Lose
parseResult "Y" = Just Draw
parseResult "Z" = Just Win
parseResult _ = Nothing

computePointsFromGestures :: (Maybe Gesture, Maybe Gesture) -> Maybe Int 
computePointsFromGestures (mTheirs, mMine) = 
  case (mTheirs, mMine) of
    (Nothing, _) -> Nothing
    (_ , Nothing) -> Nothing
    (Just theirs, Just mine) -> Just $ pointsFromResult (getResultFrom theirs mine) + pointsFromGesture mine 

getResultFrom :: Gesture -> Gesture -> Result
getResultFrom Rock Paper = Win
getResultFrom Rock Rock = Draw
getResultFrom Rock Scissors = Lose
getResultFrom Paper Scissors = Win
getResultFrom Paper Paper = Draw
getResultFrom Paper Rock = Lose
getResultFrom Scissors Rock = Win
getResultFrom Scissors Scissors = Draw
getResultFrom Scissors Paper = Lose

computePointsFromGestureAndResult :: (Maybe Gesture, Maybe Result) -> Maybe Int
computePointsFromGestureAndResult (mGesture, mResult) =
  case (mGesture, mResult) of
    (Nothing, _) -> Nothing
    (_ , Nothing) -> Nothing
    (Just gesture, Just result) -> Just $ case result of
      Win -> pointsFromGesture (winAgainst gesture) + pointsFromResult Win
      Draw -> pointsFromGesture (drawAgainst gesture) + pointsFromResult Draw
      Lose -> pointsFromGesture (loseAgainst gesture) + pointsFromResult Lose

winAgainst :: Gesture -> Gesture
winAgainst Rock = Paper
winAgainst Paper = Scissors
winAgainst Scissors = Rock

loseAgainst :: Gesture -> Gesture
loseAgainst Rock = Scissors
loseAgainst Paper = Rock
loseAgainst Scissors = Paper

drawAgainst :: Gesture -> Gesture
drawAgainst = id

pointsFromGesture :: Gesture -> Int
pointsFromGesture Rock = 1
pointsFromGesture Paper = 2
pointsFromGesture Scissors = 3 

pointsFromResult :: Result -> Int
pointsFromResult Win = 6
pointsFromResult Draw = 3
pointsFromResult Lose = 0