module PokerHands

type HandRank = HighCard | Pair | TwoPairs

let private countGroupsPairs (cards: string) =
  cards.Split(' ')
  |> Array.map (fun card -> card[0])
  |> Array.groupBy id
  |> Array.filter (fun (_, group) -> group.Length = 2)
  |> Array.length

let private isPair (cards: string) = countGroupsPairs cards = 1
  
let private isTwoPairs (cards: string) = countGroupsPairs cards = 2

let rank (cards: string) =
  if isPair cards then Pair
  elif isTwoPairs cards then TwoPairs
  else HighCard
