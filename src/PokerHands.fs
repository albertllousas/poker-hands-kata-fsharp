module PokerHands

type HandRank = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush

type Player = P1 | P2

type Winner = Winner of player: Player * rank: HandRank * hand: string list * kicker: string option

let private groupByValueAndCount (cards: string list) =
  cards
  |> List.map (fun card -> card.Substring(0, card.Length - 1))
  |> List.groupBy id
  |> List.map (fun (_, group) -> group.Length)
  |> List.sortDescending

let private mapCardValues (aceValue: int) (hand: string list)  =
  List.map (fun (card:string) -> card.Substring(0, card.Length - 1)) hand
  |> List.map (fun card -> match card with "A" -> aceValue | "K" -> 13 | "Q" -> 12 | "J" -> 11 | _ -> int card )

let private haveConsecutiveValues (cards: string list) = 
  let consecutiveCards aceValue =
    cards
    |> mapCardValues aceValue
    |> List.sort
    |> List.pairwise
    |> List.forall (fun (a, b) -> b = a + 1)
  consecutiveCards 1 || consecutiveCards 14
  
let private haveSameSuit (cards: string list) =
  cards |> List.map (fun card -> card[card.Length - 1]) |> List.distinct |> List.length = 1
  
let private rmDupValues (hand1: string list) (hand2: string list ) =
  let hand2Values = hand2 |> List.map (fun card -> card.Substring(0, card.Length - 1))
  hand1
  |> List.filter (fun card -> not (List.contains (card.Substring(0, card.Length - 1)) hand2Values ))

let valueToCard (value: int) =
  match value with | 14 -> "A" | 1 -> "A" | 13 -> "K" | 12 -> "Q" | 11 -> "J" | _ -> value.ToString()
 
let private breakTie (p1Hand, p1Rank) (p2Hand, p2Rank) =
  let highestCard (hand: string list) = hand |> mapCardValues 14 |> List.max
  if highestCard p1Hand > highestCard p2Hand then Winner(player = P1, rank = p1Rank, hand = p1Hand, kicker = None)
  elif highestCard p1Hand < highestCard p2Hand then Winner(player = P2, rank = p2Rank, hand = p2Hand, kicker = None)
  else
    let p1Kicker = highestCard (rmDupValues p1Hand p2Hand)
    let p2Kicker = highestCard (rmDupValues p2Hand p1Hand)
    if p1Kicker > p2Kicker then Winner(player = P1, rank = p1Rank, hand = p1Hand, kicker = Some (valueToCard p1Kicker))
    else Winner(player = P2, rank = p2Rank, hand = p2Hand, kicker = Some (valueToCard p2Kicker))

let rank (hand: string list) =
  if haveSameSuit hand && haveConsecutiveValues hand then StraightFlush
  elif groupByValueAndCount hand = [4; 1] then FourOfAKind
  elif groupByValueAndCount hand = [3; 2] then FullHouse
  elif haveSameSuit hand then Flush
  elif haveConsecutiveValues hand then Straight
  elif groupByValueAndCount hand = [3; 1; 1] then ThreeOfAKind
  elif groupByValueAndCount hand = [2; 2; 1] then TwoPairs
  elif groupByValueAndCount hand = [2; 1; 1; 1] then Pair
  else HighCard
  
let decideWinner (p1Hand: string list) (p2Hand: string list) : Winner =
  let idx rank = List.findIndex (fun r -> r = rank) [HighCard; Pair; TwoPairs; ThreeOfAKind; Straight; Flush; FullHouse; FourOfAKind; StraightFlush]
  let p1Rank = rank p1Hand  
  let p2Rank = rank p2Hand
  if idx p1Rank > idx p2Rank then Winner(player = P1, rank = p1Rank, hand = p1Hand, kicker = None)
  elif idx p1Rank < idx p2Rank then  Winner(player = P2, rank = p2Rank, hand = p2Hand, kicker = None)
  else breakTie (p1Hand, p1Rank) (p2Hand, p2Rank)
  