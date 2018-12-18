-- Sara Salloum and Neil Nicholson


import System.Random
import Control.Monad
import System.IO.Unsafe





data Suits = Club | Spade | Heart |Diamond
   deriving(Show, Enum, Eq)

data Values = Two | Three | Four | Five | Six | Seven |Eight |Nine |Ten |Jack |Queen |King |Ace
   deriving(Show, Ord, Enum, Eq)

makeDeck = [(value, suit) | suit <- [Club , Spade, Heart, Diamond], value  <- [Two .. Ace] ]

isBigger:: (Values, Suits) -> (Values, Suits) -> Bool
isBigger x y 
  |fst x > fst y = True
  |otherwise = False 

isEqual :: (Values, Suits) -> (Values, Suits) -> Bool
isEqual x y 
  |fst x == fst y = True
  |otherwise = False

firstHalf ::  [(Values, Suits)] ->  [(Values, Suits)]
firstHalf ls=take 26 ls

secondHalf :: [(Values, Suits)] ->  [(Values, Suits)]
secondHalf ls = drop 26 ls

splitDeck :: [(Values, Suits)] -> ([(Values, Suits)],[(Values, Suits)])
splitDeck ls = (firstHalf ls, secondHalf ls)

mix :: [(Values, Suits)] -> [(Values, Suits)]-> [(Values, Suits)] 
mix xs  [] = xs
mix []   ys = ys
mix (x:xs) (y:ys) = y : x : mix xs ys

shuffle::  [(Values, Suits)] ->  [(Values, Suits)]
shuffle list = mix (firstHalf list) (secondHalf list)


shuffleRandom = do
 num <- getStdRandom (randomR(6, 51))
 let newDeck = (iterate shuffle makeDeck!!num)
 return newDeck
 

play :: ([(Values, Suits)], [(Values, Suits)]) -> ([(Values, Suits)], [(Values, Suits)])
play tuple
  |isEqual (head(fst tuple)) (head(snd tuple)) = war tuple
  |isBigger (head(fst tuple)) (head(snd tuple)) = ((drop 1(fst tuple)) ++ [(head (fst tuple)),(head (snd tuple))], drop 1(snd tuple))
  |isBigger (head(snd tuple)) (head(fst tuple))= ((drop 1(fst tuple)) , drop 1(snd tuple)++ [(head (snd tuple)),(head (fst tuple))])

war :: ([(Values, Suits)], [(Values, Suits)]) -> ([(Values, Suits)], [(Values, Suits)])
war tuple
  |isEqual h2 h1 = play (drop 4(fst tuple), drop 4(snd tuple) )
  |isBigger h2 h1 = (drop 4(fst tuple) , drop 4 (snd tuple)++ (take 4 (snd tuple)) ++ (take 4 (fst tuple)))
  |isBigger h1 h2 = (drop 4(fst tuple) ++ (take 4 (fst tuple)) ++ (take 4 (snd tuple)), (drop 4 (snd tuple))) 
    where
    h2 = head(drop 4(snd tuple))
    h1 = head(drop 4(fst tuple))



result p1 p2 = do
  if isEqual p1 p2
    then
      
      print("ITS A WAR!!")
    
    else
      if isBigger p1 p2
        then putStrLn "Player 1 gets the cards"
        else putStrLn "Player 2 gets the cards"



defplayer deck= do
  return (fst (splitDeck deck),snd (splitDeck deck))

plays players= do 

  print("______1_____")
  print (fst players)
  print ("______2_______")
  print (snd players)
  if isEqual (head(fst players)) (head(snd players))

    then if  length (fst players) < 5 || length (snd players) < 5

            then if length (fst players) < 5
                    then print("Player 2 Wins!")
                 else
                  print ("Player 1 Wins!")

         else if isEqual (head(drop 4 (fst players))) (head (drop 4 (snd players)))
               then if  length (drop 4 (fst players)) < 5 || length  (drop 4 (snd players)) < 5

                       then if length (drop 4 (fst players)) < 5
                              then print("Player 2 Wins!")
                            else
                               print ("Player 1 Wins!")
                    else do
                        print("-------------")
                        print("New Round")
                        print ("player 1 plays")
                        print(head(fst players))
                        print ("player 2 plays ")
                        print(head(snd players))
                        resulty players  
                        plays ((play (players)))
              else if  length (fst players) == 1 || length (snd players) == 1
                      then if  isBigger (head (fst players)) (head(snd players))
                            then print("Player 1 Wins!")
                           else print ("Player 2 Wins!")
                   else do
                     print("-------------")
                     print("New Round")
                     print ("player 1 plays ")
                     print(head(fst players))
                     print ("player 2 plays ")
                     print(head(snd players))
                     resulty players
                     plays ((play (players)))
  else if  length (fst players) == 1 || length (snd players) == 1
         then if  isBigger (head (fst players)) (head(snd players))
                then print("Player 1 Wins!")
              else print ("Player 2 Wins!")
       else do
                  print("-------------")
                  print("New Round")
                  print ("player 1 plays ")
                  print(head(fst players))
                  print ("player 2 plays ")
                  print(head(snd players))
                  resulty players
                  plays ((play (players)))

autoWar = do
  deck <- shuffleRandom
  players <- defplayer deck
  go <- plays players
  return go
  

resulty players = do
  res <-result(head(fst players)) (head(snd players))
  return res

prompt = do
  print (("Press enter to play next card") )
  x <- getLine
  print("-------------")
  print("New Round")

playsInteractive players= do
  prompt
  
  
  print("______1_____")
  print (fst players)
  print ("______2_______")
  print (snd players)
  if isEqual (head(fst players)) (head(snd players))

    then if  length (fst players) < 5 || length (snd players) < 5

            then if length (fst players) < 5
                    then print("You Lose!")
                 else print ("You Win!")
         else do
          print ("You play")
          print(head(fst players))
          print ("player 2 plays ")
          print(head(snd players))
          resulty players
          playsInteractive ((play (players)))
  else if  length (fst players) == 1 || length (snd players) == 1
         then if  isBigger (head (fst players)) (head(snd players))
                then print("You Win!")
              else print ("You Loose!")
       else do
        print ("You play")
        print(head(fst players))
        print ("player 2 plays ")
        print(head(snd players))
        resulty players
        playsInteractive ((play (players)))


interactiveWar = do
  deck <- shuffleRandom
  players <- defplayer deck
  go <- playsInteractive players
  return go


