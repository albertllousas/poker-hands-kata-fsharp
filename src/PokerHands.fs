module PokerHands

type HandRank = HighCard | Pair

let private isPair (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[0])
  |> Array.groupBy id
  |> Array.exists (fun (_, group) -> group.Length = 2)

let rank (cards: string) =
  if isPair cards
  then Pair
  else HighCard
