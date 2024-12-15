module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind

let private groupByValueAndCount (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[0])
  |> Array.groupBy id
  |> Array.map (fun (_, group) -> group.Length)
  |> Array.sortDescending
  |> Array.toList

let private isPair (cards: string) = groupByValueAndCount cards = [2; 1; 1; 1]
  
let private isTwoPairs (cards: string) = groupByValueAndCount cards = [2; 2; 1]

let private isThreeOfAKind (cards: string) = groupByValueAndCount cards = [3; 1; 1]

let private isFullHouse (cards: string) = groupByValueAndCount cards = [3; 2]

let private isFourOfAKind (cards: string) = groupByValueAndCount cards = [4; 1]

let rank (cards: string) =
  if isPair cards then Pair
  elif isTwoPairs cards then TwoPairs
  elif isThreeOfAKind cards then ThreeOfAKind
  elif isFullHouse cards then FullHouse
  elif isFourOfAKind cards then FourOfAKind
  else HighCard
