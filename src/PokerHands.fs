module PokerHands

type HandRank = HighCard | Pair | TwoPairs

let private isPair (cards: string) =
  let groupsOfTwo =
    cards.Split(' ')
    |> Array.map (fun card -> card[0])
    |> Array.groupBy id
    |> Array.filter (fun (_, group) -> group.Length = 2)
    |> Array.length
  groupsOfTwo = 1
  
let private isTwoPairs (cards: string) =
  let groupsOfTwo =
    cards.Split(' ')
    |> Array.map (fun card -> card[0])
    |> Array.groupBy id
    |> Array.filter (fun (_, group) -> group.Length = 2)
    |> Array.length
  groupsOfTwo = 2

let rank (cards: string) =
  if isPair cards then Pair
  elif isTwoPairs cards then TwoPairs
  else HighCard
