module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind

let private groupByValueAndCount (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card.Substring(0, card.Length - 1))
  |> Array.groupBy id
  |> Array.map (fun (_, group) -> group.Length)
  |> Array.sortDescending
  |> Array.toList
  
let private haveConsecutiveValues (cards: string) = 
  let consecutiveCards aceValue =
    cards.Split(' ')
    |> Array.map (fun card -> card.Substring(0, card.Length - 1))
    |> Array.map (fun card -> match card with "A" -> aceValue | "K" -> 13 | "Q" -> 12 | "J" -> 11 | _ -> int card )
    |> Array.sort
    |> Array.pairwise
    |> Array.forall (fun (a, b) -> b = a + 1)
  consecutiveCards 1 || consecutiveCards 14
  
let private haveSameSuit (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[card.Length - 1])
  |> Array.distinct
  |> Array.length = 1  

let rank (cards: string) =
  if groupByValueAndCount cards = [4; 1] then FourOfAKind
  elif groupByValueAndCount cards = [3; 2] then FullHouse
  elif haveSameSuit cards then Flush
  elif haveConsecutiveValues cards then Straight
  elif groupByValueAndCount cards = [3; 1; 1] then ThreeOfAKind
  elif groupByValueAndCount cards = [2; 2; 1] then TwoPairs
  elif groupByValueAndCount cards = [2; 1; 1; 1] then Pair
  else HighCard
