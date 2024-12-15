module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind

let private groupByValueAndCount (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[0])
  |> Array.groupBy id
  |> Array.map (fun (_, group) -> group.Length)
  |> Array.sortDescending
  |> Array.toList

let rank (cards: string) =
  if groupByValueAndCount cards = [2; 1; 1; 1] then Pair
  elif groupByValueAndCount cards = [2; 2; 1] then TwoPairs
  elif groupByValueAndCount cards = [3; 1; 1] then ThreeOfAKind
  elif groupByValueAndCount cards = [3; 2] then FullHouse
  elif groupByValueAndCount cards = [4; 1] then FourOfAKind
  else HighCard
