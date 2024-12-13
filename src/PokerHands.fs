module PokerHands

type HandRank = HighCard | Pair

let rank (cards: string) =
  if cards.Split(' ')
    |> Array.map (fun card -> card[0])
    |> Array.groupBy id
    |> Array.exists (fun (_, group) -> group.Length = 2)
  then Pair
  else HighCard
