-- Sara Salloum and Neil Nicholson


import System.Random
import Control.Monad
import System.IO.Unsafe



fstOfThree :: ([(Values, Suits)],[(Values, Suits)],[(Values, Suits)]) -> [(Values, Suits)]
fstOfThree (a,b,c) = a

sndOfThree :: ([(Values, Suits)],[(Values, Suits)],[(Values, Suits)]) -> [(Values, Suits)]
sndOfThree (a,b,c) = b

thrOfThree :: ([(Values, Suits)],[(Values, Suits)],[(Values, Suits)]) -> [(Values, Suits)]
thrOfThree (a,b,c) = c

data Suits = Club | Spade | Heart |Diamond
   deriving(Show, Enum, Eq)

data Values = Two | Three | Four | Five | Six | Seven |Eight |Nine |Ten |Jack |Queen |King |Ace |Ace1
   deriving(Show, Ord, Enum, Eq)

makeDeck = [(value, suit) | suit <- [Club , Spade, Heart, Diamond], value  <- [Two .. Ace] ]

firstHalf ::  [(Values, Suits)] ->  [(Values, Suits)]
firstHalf ls=take 26 ls

secondHalf :: [(Values, Suits)] ->  [(Values, Suits)]
secondHalf ls = drop 26 ls

mix :: [(Values, Suits)] -> [(Values, Suits)]-> [(Values, Suits)] 
mix xs  [] = xs
mix []   ys = ys
mix (x:xs) (y:ys) = y : x : mix xs ys

shuffle::  [(Values, Suits)] ->  [(Values, Suits)]
shuffle list = mix (firstHalf list) (secondHalf list)

shuffleRandom = do
 num <- getStdRandom (randomR(8, 51))
 let newDeck = (iterate shuffle makeDeck!!num)
 return newDeck

eval :: (Values, Suits) -> Integer
eval tuple
  |fst tuple == Two = 2
  |fst tuple == Three = 3
  |fst tuple == Four = 4
  |fst tuple == Five = 5
  |fst tuple == Six = 6
  |fst tuple == Seven = 7
  |fst tuple == Eight = 8
  |fst tuple == Nine = 9
  |fst tuple == Ten = 10
  |fst tuple == Jack = 10
  |fst tuple == Queen = 10
  |fst tuple == King = 10
  |fst tuple == Ace = 11
  |fst tuple == Ace1 = 1

dealCards :: [(Values, Suits)] -> ([(Values, Suits)],[(Values, Suits)],[(Values, Suits)])
dealCards deck = ([head deck, head (drop 1 deck)], [head (drop 2 deck), head (drop 3 deck)], drop 4 deck)

evaluate :: [(Values, Suits)] -> Integer
evaluate ls
  | ls == [] = 0
  | length (ls) == 1 = eval (head ls)
  | length (ls) > 1 = eval (head ls) + evaluate (tail ls)

hit :: [(Values, Suits)] -> [(Values, Suits)] -> ([(Values, Suits)],[(Values, Suits)])
hit p1 deck = (p1 ++ [(head deck)], drop 1 deck) 

ifAce :: [(Values, Suits)] -> Bool
ifAce ls
 |ls == [] = False
 |fst(head ls) == Ace = True 
 |fst(head ls) /= Ace = ifAce (tail ls)

replaceAce :: [(Values, Suits)] -> [(Values, Suits)]
replaceAce ls
  |fst(head ls) == Ace = drop 1 ls ++ [(Ace1, snd(head ls))]
  |fst(head ls) /= Ace = replaceAce ((tail ls) ++ [(head ls)] )



evalHand tuple = do
 let player = fstOfThree (tuple)
 let dealer = sndOfThree (tuple)
 let current = thrOfThree (tuple) 
 print("----dealer----")
 print(head dealer)
 print("----player----")
 print(player)
 print("------")
 print("you have")
 print(evaluate player)
 if evaluate player >= 21
   then if evaluate player == 21
          then print("You Win!!")
        else if (evaluate player > 21) && (ifAce player == False)
              then do
                print("------")
                print("You have")
                print(evaluate player)
                print("You Bust!")
             else if  evaluate player > 21 && ifAce player
                   then
           
                   evalHand ((replaceAce player), dealer , (drop 1 current))
                  else
                   print ("hi Dave :) ")
 else 
   callHit player dealer current
        
blackjack = do
  deck <- shuffleRandom
  let x = dealCards deck
  go <- evalHand x
  return go


callHit player dealer current = do
  putStrLn("Do you want another card? yes/no")
  answer <- getLine
  if answer == "yes"
      then do
        print (fst(hit player current))
        print("----player-------")
        
        evalHand (fst(hit player current), dealer, (drop 1 current))
        
  else do
      print("------dealer-----")
      print dealer
      print(evaluate dealer)
      dealersTurn player dealer current


dealersTurn player dealer current = do
  if evaluate dealer < 17
    then do
     evaldealerHand (player, (fst (hit dealer current)), (snd (hit dealer current)))
  else if evaluate dealer == 21
        then print("You Lose :( The dealer got 21")
       else if evaluate dealer > evaluate player
              then do 
               print("------player-----")
               print player
               print("------dealer-----")
               print dealer
               print ("You Lose :( The dealer had more")
            else do
              print("------player-----")
              print player
              print("------dealer-----")
              print dealer
              print ("You Win! You had more or you tied")


evaldealerHand tuple = do
 let player = fstOfThree (tuple)
 let dealer = sndOfThree (tuple)
 let current = thrOfThree (tuple) 
 
 if evaluate dealer >= 21
   then if evaluate dealer == 21
          then do
            print dealer
            print(evaluate dealer)
            print("You lose!! Dealer got 21")
        else if (evaluate dealer > 21) && (ifAce dealer == False)
              then do
                print dealer
                print(evaluate dealer)
                print("You win! The dealer busted")
             else if  evaluate dealer > 21 && ifAce dealer
                   then
           
                   evaldealerHand (player, (replaceAce dealer) , (drop 1 current))
             else do
              --when less than 21 do
               print ("-----")
               print ("Dealer has")

               print (evaluate dealer)
 else 
   dealersTurn player dealer current

