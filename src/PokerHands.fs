module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind

let private countGroupsOfPairs (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[0])
  |> Array.groupBy id
  |> Array.filter (fun (_, group) -> group.Length = 2)
  |> Array.length

let private isPair (cards: string) = countGroupsOfPairs cards = 1
  
let private isTwoPairs (cards: string) = countGroupsOfPairs cards = 2

let private isThreeOfAKind (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[0])
  |> Array.groupBy id
  |> Array.exists (fun (_, group) -> group.Length = 3)

let rank (cards: string) =
  if isPair cards then Pair
  elif isTwoPairs cards then TwoPairs
  elif isThreeOfAKind cards then ThreeOfAKind
  else HighCard
