module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind

let private countGroupedRanksOfSize (size: int) (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[0])
  |> Array.groupBy id
  |> Array.filter (fun (_, group) -> group.Length = size)
  |> Array.length

let private isPair (cards: string) = countGroupedRanksOfSize 2 cards = 1
  
let private isTwoPairs (cards: string) = countGroupedRanksOfSize 2 cards = 2

let private isThreeOfAKind (cards: string) = countGroupedRanksOfSize 3 cards = 1

let rank (cards: string) =
  if isPair cards then Pair
  elif isTwoPairs cards then TwoPairs
  elif isThreeOfAKind cards then ThreeOfAKind
  else HighCard
